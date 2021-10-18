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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, YesNo}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, DraftClaim, Error, upscan => _}
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
class EnterClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val config: Configuration,
  enterClaimPage: pages.enter_claim,
  checkClaimSummaryPage: pages.check_claim_summary
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[ClaimsAnswer] = _.claimsAnswer

  def startClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (fillingOutClaim, _) =>
        generateClaimsFromDuties(fillingOutClaim.draftClaim).map(ClaimsAnswer(_)) match {
          case Left(error)         =>
            logger.warn("Error generating claims: ", error)
            Redirect(routes.SelectDutiesController.selectDuties())
          case Right(None)         =>
            logger.warn("No duties found to create claims ")
            Redirect(routes.SelectDutiesController.selectDuties())
          case Right(Some(claims)) =>
            val nextPage = calculateNextPage(claims)
            updateClaimAnswer(claims, fillingOutClaim, nextPage)
        }
      }
    }

  def enterClaim(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (_, answer) =>
        answer
          .flatMap(_.find(_.id === id))
          .map { claim =>
            val emptyForm = mrnClaimAmountForm(claim.paidAmount)
            val form      = Either.cond(claim.isFilled, emptyForm.fill(ClaimAmount(claim.claimAmount)), emptyForm).merge
            Future.successful(Ok(enterClaimPage(id, form, claim)))
          }
          .getOrElse(Future.successful(Redirect(baseRoutes.IneligibleController.ineligible())))
      }
    }

  def enterClaimSubmit(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (fillingOutClaim, answers) =>
        answers match {
          case None         =>
            Redirect(routes.EnterClaimController.startClaim())
          case Some(claims) =>
            claims.find(_.id === id) match {
              case None        =>
                Redirect(routes.EnterClaimController.startClaim())
              case Some(claim) =>
                mrnClaimAmountForm(claim.paidAmount)
                  .bindFromRequest()
                  .fold(
                    formWithErrors => {
                      val updatedErrors = formWithErrors.errors.map(d => d.copy(key = "enter-claim"))
                      BadRequest(enterClaimPage(id, formWithErrors.copy(errors = updatedErrors), claim))
                    },
                    formOk => {
                      val newClaim = claim.copy(claimAmount = formOk.amount, isFilled = true)
                      replaceUpdateRedirect(claims, newClaim, fillingOutClaim)
                    }
                  )
            }
        }
      }
    }

  def checkClaimSummary(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (_, answers) =>
        answers match {
          case Some(claims) => Ok(checkClaimSummaryPage(claims, whetherClaimCorrect))
          case None         => Redirect(routes.EnterClaimController.startClaim())
        }
      }
    }

  def checkClaimSummarySubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (fillingOutClaim: FillingOutClaim, _) =>
        whetherClaimCorrect
          .bindFromRequest()
          .fold(
            formWithErrors =>
              fillingOutClaim.draftClaim.claimsAnswer
                .map(claims => Future.successful(BadRequest(checkClaimSummaryPage(claims, formWithErrors))))
                .getOrElse(Future.successful(errorHandler.errorResult())),
            {
              case Yes =>
                fillingOutClaim.draftClaim match {
                  case claim: DraftClaim if isCmaEligible(claim) =>
                    Redirect(reimbursementRoutes.ReimbursementMethodController.showReimbursementMethod())
                  case _                                         =>
                    Redirect(routes.BankAccountController.checkBankAccountDetails(extractJourney))
                }

              case No => Redirect(routes.SelectDutiesController.selectDuties())
            }
          )
      }
    }

  protected def calculateNextPage(claimAnswer: ClaimsAnswer): Result =
    claimAnswer.find(_.isFilled === false) match {
      case Some(claim) =>
        Redirect(routes.EnterClaimController.enterClaim(claim.id))
      case None        =>
        Redirect(routes.EnterClaimController.checkClaimSummary())
    }

  protected def updateClaimAnswer(claimAnswer: ClaimsAnswer, fillingOutClaim: FillingOutClaim, nextPage: Result)(
    implicit request: RequestWithSessionData[AnyContent]
  ): Future[Result] = {
    val newDraftClaim  = fillingOutClaim.draftClaim.copy(claimsAnswer = Some(claimAnswer))
    val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)
    EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap(_ => Error("could not update session"))
      .fold(logAndDisplayError("could not save claims"), _ => nextPage)
  }

  protected def replaceUpdateRedirect(claimAnswer: ClaimsAnswer, newClaim: Claim, fillingOutClaim: FillingOutClaim)(
    implicit request: RequestWithSessionData[AnyContent]
  ): Future[Result] = {
    val notEditedClaims = claimAnswer.toList.filterNot(_.id === newClaim.id)
    val claims          = ClaimsAnswer(newClaim, notEditedClaims: _*)
    val nextPage        = calculateNextPage(claims)
    updateClaimAnswer(claims, fillingOutClaim, nextPage)
  }

}

object EnterClaimController {

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

  def generateClaimsFromDuties(draftC285Claim: DraftClaim): Either[Error, List[Claim]] = {
    val claims      = draftC285Claim.claimsAnswer.map(_.toList).getOrElse(Nil)
    val ndrcDetails = draftC285Claim.displayDeclaration.flatMap(_.displayResponseDetail.ndrcDetails).getOrElse(Nil)
    draftC285Claim.dutiesSelectedAnswer
      .map(_.toList)
      .toRight(Error("No duties in session when arriving on ClaimController"))
      .map(_.map { duty =>
        claims.find(claim => claim.taxCode === duty.taxCode.value) match {
          case Some(claim) => Some(claim)
          case None        => //No Claim for the given Duty, we have to create one
            ndrcDetails.find(ndrc => ndrc.taxType === duty.taxCode.value) match {
              case Some(ndrc) => Some(Claim.fromNdrc(ndrc))
              case None       => Some(Claim.fromDuty(duty))
            }
        }
      })
      .map(_.flatten(Option.option2Iterable).toList)
  }

  val checkClaimSummaryKey: String = "check-claim-summary"

  val whetherClaimCorrect: Form[YesNo] =
    YesOrNoQuestionForm(checkClaimSummaryKey)
}
