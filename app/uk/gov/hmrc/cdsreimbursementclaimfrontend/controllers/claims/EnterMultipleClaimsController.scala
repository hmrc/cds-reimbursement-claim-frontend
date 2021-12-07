/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumeral
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.ChangeFlagUtils._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterMultipleClaimsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val config: Configuration,
  enterMultipleClaimPage: pages.enter_multiple_claims,
  checkMultipleClaimSummaryPage: pages.check_multiple_claim_summary,
  mrnDoesNotExistPage: pages.mrn_does_not_exist
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  import EnterMultipleClaimsController._

  def enterClaim(mrnIndex: Int, taxCode: TaxCode): Action[AnyContent] =
    whenAuthenticatedAndValidRequest(mrnIndex, taxCode) { implicit request => _ => mrn => _ => claim =>
      val emptyForm  = correctedAmountForm(claim.paidAmount)
      val form       =
        if (claim.isFilled) emptyForm.fill(CorrectedAmount(claim.correctedAmount))
        else emptyForm
      val submitCall = routes.EnterMultipleClaimsController
        .enterClaimSubmit(mrnIndex, claim.taxCode)
        .maybeSetChangeFlag
      Ok(enterMultipleClaimPage(form, mrnIndex, mrn, claim, submitCall))
    }

  def enterClaimSubmit(mrnIndex: Int, taxCode: TaxCode): Action[AnyContent] = {
    val index = mrnIndex - 1
    whenAuthenticatedAndValidRequest(mrnIndex, taxCode) {
      implicit request => fillingOutClaim => mrn => claims => claim =>
        correctedAmountForm(claim.paidAmount)
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedErrors = formWithErrors.errors.map(d => d.copy(key = "multiple-enter-claim"))
              val submitCall    = routes.EnterMultipleClaimsController.enterClaimSubmit(mrnIndex, claim.taxCode)
              BadRequest(
                enterMultipleClaimPage(formWithErrors.copy(errors = updatedErrors), mrnIndex, mrn, claim, submitCall)
              )
            },
            formOk => {
              val correctedAmount = formOk.amount
              val newClaim        = claim.fillWithCorrectedAmount(correctedAmount)
              updateJourneyWithClaim(fillingOutClaim, index, claims, newClaim)
                .fold(
                  e => logAndDisplayError("could not update draft claim").apply(Error(e)),
                  updatedJourney =>
                    EitherT(updateSession(sessionStore, request)(_.copyWith(journeyStatus = Some(updatedJourney))))
                      .leftMap(_ => Error("could not update session"))
                      .fold(
                        logAndDisplayError("could not save claims"),
                        _ =>
                          Redirect(
                            if (isChangeRequest)
                              routes.EnterMultipleClaimsController.checkClaimSummary
                            else
                              selectNextPage(updatedJourney, mrnIndex, taxCode)
                          )
                      )
                )
            }
          )
    }
  }

  val checkClaimSummary: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        fillingOutClaim.draftClaim.MRNs.nonEmptyListOpt match {
          case None =>
            Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(Multiple))

          case Some(mrns) =>
            fillingOutClaim.draftClaim.Claims.nonEmptyListOpt match {
              case None =>
                Redirect(SelectMultipleDutiesController.selectNextPage(fillingOutClaim, 1))

              case Some(claimsList) =>
                firstMissingClaimEntry(claimsList) match {
                  case Some((mrnIndex, taxCode)) =>
                    Redirect(routes.EnterMultipleClaimsController.enterClaim(mrnIndex, taxCode))

                  case None =>
                    val mrnsWithClaimsList = mrns.zipWithIndex
                      .zipWith(claimsList) { case ((mrn, index), claim) => (index, mrn, claim) }
                    val submitCall         =
                      routes.EnterMultipleClaimsController.checkClaimSummarySubmit.maybeSetChangeFlag

                    val changeCall =
                      (mrnIndex: Int, taxCode: TaxCode) =>
                        routes.EnterMultipleClaimsController.enterClaim(mrnIndex, taxCode).setChangeFlag
                    Ok(checkMultipleClaimSummaryPage(mrnsWithClaimsList, isClaimCorrectForm, submitCall, changeCall))
                }
            }
        }
      }
    }

  val checkClaimSummarySubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        implicit val router: ReimbursementRoutes = extractRoutes(fillingOutClaim.draftClaim, JourneyBindable.Multiple)
        fillingOutClaim.draftClaim.MRNs.nonEmptyListOpt match {
          case None       =>
            Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(Multiple))
          case Some(mrns) =>
            fillingOutClaim.draftClaim.Claims.nonEmptyListOpt match {
              case None =>
                Redirect(SelectMultipleDutiesController.selectNextPage(fillingOutClaim, 1))

              case Some(claimsList) =>
                firstMissingClaimEntry(claimsList) match {
                  case Some((mrnIndex, taxCode)) =>
                    Redirect(routes.EnterMultipleClaimsController.enterClaim(mrnIndex, taxCode))

                  case None =>
                    isClaimCorrectForm
                      .bindFromRequest()
                      .fold(
                        formWithErrors => {
                          val mrnsWithClaimsList = mrns.zipWithIndex
                            .zipWith(claimsList) { case ((mrn, index), claim) => (index, mrn, claim) }
                          val submitCall         =
                            routes.EnterMultipleClaimsController.checkClaimSummarySubmit.maybeSetChangeFlag
                          val changeCall         =
                            (mrnIndex: Int, taxCode: TaxCode) =>
                              routes.EnterMultipleClaimsController.enterClaim(mrnIndex, taxCode).setChangeFlag
                          Ok(checkMultipleClaimSummaryPage(mrnsWithClaimsList, formWithErrors, submitCall, changeCall))
                        },
                        {
                          case Yes =>
                            Redirect(
                              router.CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                                routes.BankAccountController.checkBankAccountDetails(Multiple)
                              )
                            )

                          case No =>
                            Redirect(routes.SelectMultipleDutiesController.selectDuties(1))
                        }
                      )
                }
            }
        }
      }
    }

  private def whenAuthenticatedAndValidRequest(mrnIndex: Int, taxCode: TaxCode)(
    body: RequestWithSessionData[_] => FillingOutClaim => MRN => List[
      ClaimedReimbursement
    ] => ClaimedReimbursement => Future[Result]
  ): Action[AnyContent] = {
    val index = mrnIndex - 1
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case journey: FillingOutClaim =>
        journey.draftClaim.MRNs
          .get(index)
          .fold(toFuture(BadRequest(mrnDoesNotExistPage()))) { mrn =>
            (for {
              selectedDuties <- journey.draftClaim.DutiesSelections.get(index)
              dutyIndex       = selectedDuties.indexOf(Duty(taxCode))
              existingClaims <- if (dutyIndex =!= -1) journey.draftClaim.Claims.get(index).orElse(Some(Nil))
                                else None
              claims          = EnterMultipleClaimsController
                                  .prepareClaims(index, selectedDuties, existingClaims, journey)
              claim          <- claims.drop(dutyIndex).headOption
            } yield body(request)(journey)(mrn)(claims)(claim))
              .getOrElse(
                toFuture(Redirect(routes.SelectMultipleDutiesController.selectDuties(mrnIndex)))
              )
          }
      }
    }
  }
}

object EnterMultipleClaimsController {

  final case class CorrectedAmount(amount: BigDecimal)

  def selectNextPage(
    fillingOutClaim: FillingOutClaim,
    mrnIndex: Int,
    taxCode: TaxCode
  ): Call =
    fillingOutClaim.draftClaim.DutiesSelections
      .get(mrnIndex - 1)
      .fold(routes.SelectMultipleDutiesController.selectDuties(mrnIndex)) { duties =>
        val currentDuty = Duty(taxCode)
        val nextDutyOpt = {
          val dutyIndex = duties.indexOf(currentDuty)
          if (dutyIndex === -1) None
          else duties.drop(dutyIndex + 1).headOption
        }
        nextDutyOpt match {
          case Some(nextDuty) =>
            routes.EnterMultipleClaimsController.enterClaim(mrnIndex, nextDuty.taxCode)
          case None           =>
            if (mrnIndex < fillingOutClaim.draftClaim.MRNs.total)
              routes.SelectMultipleDutiesController.selectDuties(mrnIndex + 1)
            else
              routes.EnterMultipleClaimsController.checkClaimSummary
        }
      }

  def prepareClaims(
    index: Int,
    selectedDuties: List[Duty],
    existingClaims: List[ClaimedReimbursement],
    journey: FillingOutClaim
  ): List[ClaimedReimbursement] = {

    val ndrcDetails: Option[List[NdrcDetails]] =
      journey.draftClaim.Declarations
        .get(index)
        .flatMap(_.displayResponseDetail.ndrcDetails)

    selectedDuties.map { duty =>
      existingClaims.find(_.taxCode === duty.taxCode) match {
        case Some(claim) if duty.taxCode === claim.taxCode =>
          claim

        case None =>
          ndrcDetails
            .flatMap(
              _.find(ndrc => ndrc.taxType === duty.taxCode.value)
                .flatMap(ndrc => ClaimedReimbursement.fromNdrc(ndrc))
            )
            .getOrElse(ClaimedReimbursement.fromDuty(duty))
      }
    }
  }

  def updateJourneyWithClaim(
    journey: FillingOutClaim,
    index: Int,
    claims: List[ClaimedReimbursement],
    newClaim: ClaimedReimbursement
  ): Either[String, FillingOutClaim] =
    NonEmptyList
      .fromList(claims)
      .replaceOrAppend(claims.indexWhere(_.taxCode === newClaim.taxCode), newClaim)
      .flatMap { claimsAnswer =>
        if (index === 0) {
          Right(journey.copy(draftClaim = journey.draftClaim.copy(claimedReimbursementsAnswer = claimsAnswer)))
        } else
          claimsAnswer
            .map { ca =>
              journey.draftClaim.associatedMRNsClaimsAnswer
                .replaceOrAppend(index - 1, ca)
                .map { associatedMRNsClaimsAnswer =>
                  journey
                    .copy(draftClaim = journey.draftClaim.copy(associatedMRNsClaimsAnswer = associatedMRNsClaimsAnswer))
                }
            }
            .getOrElse(Left(s"Missing claims answer for ${OrdinalNumeral(index + 1)} MRN."))
      }

  def firstMissingClaimEntry(claimsList: NonEmptyList[NonEmptyList[ClaimedReimbursement]]): Option[(Int, TaxCode)] =
    claimsList.zipWithIndex
      .collect { case (claims, index) =>
        claims.find(claim => !claim.isFilled).map(claim => (index + 1, claim.taxCode))
      }
      .headOption
      .flatten

  def correctedAmountForm(paidAmount: BigDecimal): Form[CorrectedAmount] =
    Form(
      mapping(
        "multiple-enter-claim" -> moneyMapping(13, 2, "actual-amount.error.invalid", allowZero = true)
      )(CorrectedAmount.apply)(CorrectedAmount.unapply)
        .verifying("invalid.claim", a => a.amount >= 0 && a.amount < paidAmount)
    )

  val isClaimCorrectForm: Form[YesNo] =
    YesOrNoQuestionForm("multiple-check-claim-summary")

}
