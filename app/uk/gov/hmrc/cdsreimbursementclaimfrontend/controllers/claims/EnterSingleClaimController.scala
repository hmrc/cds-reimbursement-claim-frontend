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
import cats.implicits.{catsSyntaxEq, _}
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.extractJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterSingleClaimController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimedReimbursementsAnswer, YesNo}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ClaimedReimbursement, DraftClaim, Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterSingleClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val config: Configuration,
  enterSingleClaimPage: pages.enter_single_claim,
  checkSingleClaimSummaryPage: pages.check_single_claim_summary
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[ClaimedReimbursementsAnswer] = _.claimedReimbursementsAnswer

  def startClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimedReimbursementsAnswer] { (fillingOutClaim, _) =>
        generateReimbursementsFromDuties(fillingOutClaim.draftClaim).map(ClaimedReimbursementsAnswer(_)) match {
          case Left(error)         =>
            logger.warn("Error generating claims: ", error)
            Redirect(routes.SelectDutiesController.selectDuties())
          case Right(None)         =>
            logger.warn("No duties found to create claims ")
            Redirect(routes.SelectDutiesController.selectDuties())
          case Right(Some(claims)) =>
            val nextPage = calculateNextPage(claims)
            updateClaimedReimbursementsAnswer(claims, fillingOutClaim, nextPage)
        }
      }
    }

  def enterClaim(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimedReimbursementsAnswer] { (_, answer) =>
        answer
          .flatMap(_.find(_.id === id))
          .map { claim =>
            val emptyForm = mrnClaimAmountForm(claim.paidAmount)
            val form      = Either.cond(claim.isFilled, emptyForm.fill(ClaimAmount(claim.claimAmount)), emptyForm).merge
            Future.successful(Ok(enterSingleClaimPage(id, form, claim)))
          }
          .getOrElse(Future.successful(Redirect(baseRoutes.IneligibleController.ineligible())))
      }
    }

  def enterClaimSubmit(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimedReimbursementsAnswer] { (fillingOutClaim, answers) =>
        answers match {
          case None                 =>
            Redirect(routes.EnterSingleClaimController.startClaim())
          case Some(reimbursements) =>
            reimbursements.find(_.id === id) match {
              case None                =>
                Redirect(routes.EnterSingleClaimController.startClaim())
              case Some(reimbursement) =>
                mrnClaimAmountForm(reimbursement.paidAmount)
                  .bindFromRequest()
                  .fold(
                    formWithErrors => {
                      val updatedErrors = formWithErrors.errors.map(d => d.copy(key = "enter-claim"))
                      BadRequest(enterSingleClaimPage(id, formWithErrors.copy(errors = updatedErrors), reimbursement))
                    },
                    formOk => {
                      val newClaim = reimbursement.copy(claimAmount = formOk.amount, isFilled = true)
                      replaceUpdateRedirect(reimbursements, newClaim, fillingOutClaim)
                    }
                  )
            }
        }
      }
    }

  def checkClaimSummary(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimedReimbursementsAnswer] { (_, answers) =>
        answers match {
          case Some(claims) => Ok(checkSingleClaimSummaryPage(claims, whetherClaimCorrect))
          case None         => Redirect(routes.EnterSingleClaimController.startClaim())
        }
      }
    }

  def checkClaimSummarySubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimedReimbursementsAnswer] { (fillingOutClaim: FillingOutClaim, _) =>
        whetherClaimCorrect
          .bindFromRequest()
          .fold(
            formWithErrors =>
              fillingOutClaim.draftClaim.claimedReimbursementsAnswer
                .map(claims => Future.successful(BadRequest(checkSingleClaimSummaryPage(claims, formWithErrors))))
                .getOrElse(Future.successful(errorHandler.errorResult())),
            {
              case Yes =>
                fillingOutClaim.draftClaim match {
                  case claim: DraftClaim if isCmaEligible(claim) =>
                    Redirect(routes.ReimbursementMethodController.showReimbursementMethod())
                  case _                                         =>
                    Redirect(routes.BankAccountController.checkBankAccountDetails(extractJourney))
                }

              case No => Redirect(routes.SelectDutiesController.selectDuties())
            }
          )
      }
    }

  protected def calculateNextPage(claimedReimbursements: ClaimedReimbursementsAnswer): Result =
    claimedReimbursements.find(_.isFilled === false) match {
      case Some(claim) =>
        Redirect(routes.EnterSingleClaimController.enterClaim(claim.id))
      case None        =>
        Redirect(routes.EnterSingleClaimController.checkClaimSummary())
    }

  protected def updateClaimedReimbursementsAnswer(
    reimbursements: ClaimedReimbursementsAnswer,
    fillingOutClaim: FillingOutClaim,
    nextPage: Result
  )(implicit
    request: RequestWithSessionData[AnyContent]
  ): Future[Result] = {
    val newDraftClaim  = fillingOutClaim.draftClaim.copy(claimedReimbursementsAnswer = Some(reimbursements))
    val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)
    EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap(_ => Error("could not update session"))
      .fold(logAndDisplayError("could not save claims"), _ => nextPage)
  }

  protected def replaceUpdateRedirect(
    answer: ClaimedReimbursementsAnswer,
    newReimbursement: ClaimedReimbursement,
    fillingOutClaim: FillingOutClaim
  )(implicit
    request: RequestWithSessionData[AnyContent]
  ): Future[Result] = {
    val notEditedClaims = answer.toList.filterNot(_.id === newReimbursement.id)
    val claims          = ClaimedReimbursementsAnswer(newReimbursement, notEditedClaims: _*)
    val nextPage        = calculateNextPage(claims)
    updateClaimedReimbursementsAnswer(claims, fillingOutClaim, nextPage)
  }

}

object EnterSingleClaimController {

  final case class ClaimAmount(amount: BigDecimal)

  def isCmaEligible(draftC285Claim: DraftClaim): Boolean = {
    val duties = selectedDuties(draftC285Claim)
    duties.nonEmpty && duties
      .map(_.flatMap(_.cmaEligible).getOrElse("0"))
      .forall(_ === "1")
  }

  private def selectedDuties(draftC285Claim: DraftClaim): List[Option[NdrcDetails]] = {
    val nrdcDetailsMap = draftC285Claim.displayDeclaration
      .flatMap(_.displayResponseDetail.ndrcDetails)
      .getOrElse(Nil)
      .map(duty => duty.taxType -> duty)
      .toMap
    draftC285Claim.dutiesSelectedAnswer match {
      case Some(dutiesSelectedAnswer) =>
        dutiesSelectedAnswer.toList
          .filter(duty => nrdcDetailsMap.contains(duty.taxCode.value))
          .map(duty => nrdcDetailsMap.get(duty.taxCode.value))
      case _                          =>
        Nil
    }
  }

  def mrnClaimAmountForm(paidAmount: BigDecimal): Form[ClaimAmount] =
    Form(
      mapping(
        "enter-claim" -> moneyMapping(13, 2, "claim-amount.error.invalid")
      )(ClaimAmount.apply)(ClaimAmount.unapply)
        .verifying("invalid.claim", a => a.amount <= paidAmount)
    )

  def generateReimbursementsFromDuties(draftC285Claim: DraftClaim): Either[Error, List[ClaimedReimbursement]] = {
    val claims      = draftC285Claim.claimedReimbursementsAnswer.map(_.toList).getOrElse(Nil)
    val ndrcDetails = draftC285Claim.displayDeclaration.flatMap(_.displayResponseDetail.ndrcDetails).getOrElse(Nil)
    draftC285Claim.dutiesSelectedAnswer
      .map(_.toList)
      .toRight(Error("No duties in session when arriving on ClaimController"))
      .map(_.map { duty =>
        claims.find(claim => claim.taxCode === duty.taxCode) match {
          case Some(claim) => claim
          case None        => //No Claim for the given Duty, we have to create one
            ndrcDetails
              .find(ndrc => ndrc.taxType === duty.taxCode.value)
              .flatMap(ndrc => ClaimedReimbursement.fromNdrc(ndrc))
              .getOrElse(ClaimedReimbursement.fromDuty(duty))
        }
      }.toList)
  }

  val checkClaimSummaryKey: String = "check-claim-summary"

  val whetherClaimCorrect: Form[YesNo] =
    YesOrNoQuestionForm(checkClaimSummaryKey)
}
