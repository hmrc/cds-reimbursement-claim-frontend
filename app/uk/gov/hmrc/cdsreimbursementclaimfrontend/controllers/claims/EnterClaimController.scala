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
import julienrf.json.derived
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.libs.json.OFormat
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.extractJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.{ClaimAmount, ClaimAndPaidAmount, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimsAnswer
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
  enterEntryClaimPage: pages.enter_entry_claim,
  checkClaimSummaryPage: pages.check_claim_summary
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[ClaimsAnswer] = _.claimsAnswer

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
      withAnswers[ClaimsAnswer] { (fillingOutClaim, answer) =>
        answer.flatMap(_.find(_.id === id)) match {
          case Some(claim) =>
            fillingOutClaim.draftClaim.isMrnFlow match {
              case true  =>
                val emptyForm = mrnClaimAmountForm(claim.paidAmount)
                val form      = Either.cond(claim.isFilled, emptyForm.fill(ClaimAmount(claim.claimAmount)), emptyForm).merge
                Ok(enterClaimPage(id, form, claim))
              case false =>
                val form = Either
                  .cond(
                    claim.isFilled,
                    entryClaimAmountForm.fill(ClaimAndPaidAmount(claim.paidAmount, claim.claimAmount)),
                    entryClaimAmountForm
                  )
                  .merge
                Ok(enterEntryClaimPage(id, form, claim))
            }
          case None        =>
            Ok("This claim no longer exists") //TODO replace this with the a proper error page
        }
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
                fillingOutClaim.draftClaim.isMrnFlow match {
                  case true  =>
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
                  case false =>
                    entryClaimAmountForm
                      .bindFromRequest()
                      .fold(
                        formWithErrors => {
                          val updatedErrors = formWithErrors.errors.map(d => d.copy(key = "enter-claim"))
                          BadRequest(enterEntryClaimPage(id, formWithErrors.copy(errors = updatedErrors), claim))
                        },
                        formOk => {
                          val newClaim =
                            claim
                              .copy(paidAmount = formOk.paidAmount, claimAmount = formOk.claimAmount, isFilled = true)
                          replaceUpdateRedirect(claims, newClaim, fillingOutClaim)
                        }
                      )
                }
            }
        }

      }
    }

  def checkClaimSummary(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (_, answers) =>
        answers match {
          case Some(claims) => Ok(checkClaimSummaryPage(claims, checkClaimAnswerForm))
          case None         => Redirect(routes.EnterClaimController.startClaim())
        }
      }
    }

  def checkClaimSummarySubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ClaimsAnswer] { (fillingOutClaim: FillingOutClaim, _) =>
        EnterClaimController.checkClaimAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              fillingOutClaim.draftClaim
                .fold(_.claimsAnswer)
                .map(claims => Future.successful(BadRequest(checkClaimSummaryPage(claims, formWithErrors))))
                .getOrElse(Future.successful(errorHandler.errorResult())),
            { claims =>
              val newDraftClaim  = fillingOutClaim.draftClaim.fold(_.copy(checkClaimAnswer = Some(claims)))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not get radio button details"),
                _ =>
                  claims match {
                    case ClaimAnswersAreCorrect =>
                      fillingOutClaim.draftClaim match {
                        case claim: DraftC285Claim if !claim.isMrnFlow     =>
                          Redirect(routes.BankAccountController.enterBankAccountDetails(extractJourney))
                        case claim: DraftC285Claim if isCmaEligible(claim) =>
                          Redirect(reimbursementRoutes.ReimbursementMethodController.showReimbursementMethod())
                        case _                                             =>
                          Redirect(routes.BankAccountController.checkBankAccountDetails(extractJourney))
                      }

                    case ClaimAnswersAreIncorrect => Redirect(routes.SelectDutiesController.selectDuties())
                  }
              )
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
    val newDraftClaim  = fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(claimAnswer)))
    val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)
    EitherT
      .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap((_: Unit) => Error("could not update session"))
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

  final case class ClaimAndPaidAmount(paidAmount: BigDecimal, claimAmount: BigDecimal)

  val entryClaimAmountForm: Form[ClaimAndPaidAmount] = Form(
    mapping(
      "enter-claim.paid-amount"  -> moneyMapping(13, 2, "paid-amount.error.invalid"),
      "enter-claim.claim-amount" -> moneyMapping(13, 2, "claim-amount.error.invalid")
    )(ClaimAndPaidAmount.apply)(ClaimAndPaidAmount.unapply)
      .verifying("invalid.claim", a => a.claimAmount <= a.paidAmount)
  )

  def isCmaEligible(draftC285Claim: DraftC285Claim): Boolean = {
    val duties = selectedDuties(draftC285Claim)
    duties.nonEmpty && duties
      .forall(_.isDefined) && duties.map(_.flatMap(_.cmaEligible).getOrElse("0")).forall(_ === "1")
  }

  private def selectedDuties(draftC285Claim: DraftC285Claim): List[Option[NdrcDetails]] = {
    val nrdcDetailsMap = draftC285Claim
      .fold(_.displayDeclaration)
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
        List()
    }
  }

  def mrnClaimAmountForm(paidAmount: BigDecimal): Form[ClaimAmount] =
    Form(
      mapping(
        "enter-claim" -> moneyMapping(13, 2, "claim-amount.error.invalid")
      )(ClaimAmount.apply)(ClaimAmount.unapply)
        .verifying("invalid.claim", a => a.amount <= paidAmount)
    )

  def generateClaimsFromDuties(draftC285Claim: DraftC285Claim): Either[Error, List[Claim]] = {
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
              case Some(ndrc) => Some(claimFromNdrc(ndrc))
              case None       => Some(claimFromDuty(duty))
            }
        }
      })
      .map(_.flatten(Option.option2Iterable).toList)
  }

  private def claimFromDuty(duty: Duty): Claim = Claim(
    UUID.randomUUID(),
    "001",
    "Unknown",
    duty.taxCode.value,
    BigDecimal(0.0),
    BigDecimal(0.0),
    isFilled = false
  )

  private def claimFromNdrc(ndrc: NdrcDetails): Claim = Claim(
    UUID.randomUUID(),
    ndrc.paymentMethod,
    ndrc.paymentReference,
    ndrc.taxType,
    BigDecimal(ndrc.amount),
    BigDecimal(0.0),
    isFilled = false
  )

  sealed trait CheckClaimAnswer extends Product with Serializable

  case object ClaimAnswersAreCorrect extends CheckClaimAnswer
  case object ClaimAnswersAreIncorrect extends CheckClaimAnswer

  implicit val checkClaimAnswerFormat: OFormat[CheckClaimAnswer] = derived.oformat[CheckClaimAnswer]()

  val messageKey: String = "check-claim-summary"

  val checkClaimAnswerForm: Form[CheckClaimAnswer] =
    Form(
      mapping(
        messageKey -> number
          .verifying("invalid", a => a === 0 || a === 1)
          .transform[CheckClaimAnswer](
            value =>
              if (value === 0) ClaimAnswersAreCorrect
              else ClaimAnswersAreIncorrect,
            {
              case ClaimAnswersAreCorrect   => 0
              case ClaimAnswersAreIncorrect => 1
            }
          )
      )(identity)(Some(_))
    )

}
