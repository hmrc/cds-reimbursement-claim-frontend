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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor.extractJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.{ClaimAmount, ClaimAndPaidAmount, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, Error, TaxCategory, TaxCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val config: Configuration,
  enterClaimPage: pages.enter_claim,
  enterScheduledClaimPage: pages.enter_claim_scheduled,
  checkClaimSummaryPage: pages.check_claim_summary
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[ClaimsAnswer] = _.claimsAnswer

  def startClaim(implicit journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ClaimsAnswer] { (fillingOutClaim, _, _) =>
        val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
        generateClaimsFromDuties(draftC285Claim).map(ClaimsAnswer(_)) match {
          case Left(error)         =>
            logger.warn("Error generating claims: ", error)
            Redirect(routes.SelectDutiesController.selectDuties())
          case Right(None)         =>
            logger.warn("No duties found to create claims ")
            Redirect(routes.SelectDutiesController.selectDuties())
          case Right(Some(claims)) =>
            val nextPage = calculateNextPage(claims, journeyBindable)
            updateClaimAnswer(claims, fillingOutClaim, nextPage)
        }
      }
    }

  def enterClaim(taxCategory: TaxCategory, taxCode: TaxCode, journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      implicit val journey: JourneyBindable = journeyBindable
      withAnswersAndRoutes[ClaimsAnswer] { (_, answer, _) =>
        answer.flatMap(_.find(claim => claim.taxCategory === taxCategory && claim.taxCode === taxCode)) match {
          case Some(claim) =>
            journeyBindable match {
              case JourneyBindable.Single | JourneyBindable.Bulk =>
                val emptyForm = mrnClaimAmountForm(claim.paidAmount)
                val form      = Either.cond(claim.isFilled, emptyForm.fill(ClaimAmount(claim.claimAmount)), emptyForm).merge
                Ok(enterClaimPage(form, claim))
              case JourneyBindable.Scheduled                     =>
                val form = Either
                  .cond(
                    claim.isFilled,
                    scheduledClaimAmountForm.fill(ClaimAndPaidAmount(claim.paidAmount, claim.claimAmount)),
                    scheduledClaimAmountForm
                  )
                  .merge
                Ok(enterScheduledClaimPage(form, claim))
            }
          case None        =>
            Ok("This claim no longer exists") //TODO replace this with the a proper error page
        }
      }
    }

  def enterClaimSubmit(
    taxCategory: TaxCategory,
    taxCode: TaxCode,
    journeyBindable: JourneyBindable
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      implicit val journey: JourneyBindable = journeyBindable
      withAnswers[ClaimsAnswer] { (fillingOutClaim, answers) =>
        answers match {
          case None         =>
            Redirect(routes.EnterClaimController.startClaim(journeyBindable))
          case Some(claims) =>
            claims.find(claim => claim.taxCategory === taxCategory && claim.taxCode === taxCode) match {
              case None        =>
                Redirect(routes.EnterClaimController.startClaim(journeyBindable))
              case Some(claim) =>
                journeyBindable match {
                  case JourneyBindable.Single | JourneyBindable.Bulk =>
                    mrnClaimAmountForm(claim.paidAmount)
                      .bindFromRequest()
                      .fold(
                        formWithErrors => {
                          val updatedErrors = formWithErrors.errors.map(_.copy(key = singleLanguageKey))
                          BadRequest(
                            enterClaimPage(formWithErrors.copy(errors = updatedErrors), claim)
                          )
                        },
                        formOk => {
                          val newClaim = claim.copy(claimAmount = formOk.amount, isFilled = true)
                          replaceUpdateRedirect(claims, newClaim, fillingOutClaim, journeyBindable)
                        }
                      )
                  case JourneyBindable.Scheduled                     =>
                    scheduledClaimAmountForm
                      .bindFromRequest()
                      .fold(
                        formWithErrors => {
                          val updatedErrors = formWithErrors.errors.map(_.copy(key = scheduledLanguageKey))
                          BadRequest(
                            enterScheduledClaimPage(formWithErrors.copy(errors = updatedErrors), claim)
                          )
                        },
                        formOk => {
                          val newClaim =
                            claim
                              .copy(paidAmount = formOk.paidAmount, claimAmount = formOk.claimAmount, isFilled = true)
                          replaceUpdateRedirect(claims, newClaim, fillingOutClaim, journeyBindable)
                        }
                      )
                }
            }
        }

      }
    }

  def checkClaimSummary(journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      implicit val journey: JourneyBindable = journeyBindable
      withAnswers[ClaimsAnswer] { (_, answers) =>
        answers match {
          case Some(claims) => Ok(checkClaimSummaryPage(claims, checkClaimAnswerForm))
          case None         => Redirect(routes.EnterClaimController.startClaim(journeyBindable))
        }
      }
    }

  def checkClaimSummarySubmit(journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      implicit val journey: JourneyBindable = journeyBindable
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
                      if (fillingOutClaim.draftClaim.fold(_.isMrnFlow))
                        Redirect(routes.BankAccountController.checkBankAccountDetails(extractJourney))
                      else Redirect(routes.BankAccountController.enterBankAccountDetails(extractJourney))

                    case ClaimAnswersAreIncorrect => Redirect(routes.SelectDutiesController.selectDuties())
                  }
              )
            }
          )
      }
    }

  protected def calculateNextPage(claimAnswer: ClaimsAnswer, journeyBindable: JourneyBindable): Result =
    claimAnswer.find(_.isFilled === false) match {
      case Some(claim) =>
        Redirect(routes.EnterClaimController.enterClaim(claim.taxCategory, claim.taxCode, journeyBindable))
      case None        =>
        Redirect(routes.EnterClaimController.checkClaimSummary(journeyBindable))
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

  protected def replaceUpdateRedirect(
    claimAnswer: ClaimsAnswer,
    newClaim: Claim,
    fillingOutClaim: FillingOutClaim,
    journeyBindable: JourneyBindable
  )(implicit
    request: RequestWithSessionData[AnyContent]
  ): Future[Result] = {
    val notEditedClaims = claimAnswer.toList.filterNot(oldClaim =>
      oldClaim.taxCategory === newClaim.taxCategory && oldClaim.taxCode === newClaim.taxCode
    )
    val claims          = ClaimsAnswer(newClaim, notEditedClaims: _*)
    val nextPage        = calculateNextPage(claims, journeyBindable)
    updateClaimAnswer(claims, fillingOutClaim, nextPage)
  }

}

object EnterClaimController {

  final case class ClaimAmount(amount: BigDecimal)

  final case class ClaimAndPaidAmount(paidAmount: BigDecimal, claimAmount: BigDecimal)

  val scheduledLanguageKey = "enter-claim.scheduled"

  val scheduledClaimAmountForm: Form[ClaimAndPaidAmount] = Form(
    mapping(
      s"$scheduledLanguageKey.paid-amount"  -> moneyMapping(13, 2, "paid-amount.error.invalid"),
      s"$scheduledLanguageKey.claim-amount" -> moneyMapping(13, 2, "claim-amount.error.invalid")
    )(ClaimAndPaidAmount.apply)(ClaimAndPaidAmount.unapply)
      .verifying("invalid.claim", a => a.claimAmount <= a.paidAmount)
  )

  val singleLanguageKey = "enter-claim"

  def mrnClaimAmountForm(paidAmount: BigDecimal): Form[ClaimAmount] =
    Form(
      mapping(
        singleLanguageKey -> moneyMapping(13, 2, "claim-amount.error.invalid")
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
        claims.find(claim => claim.taxCode === duty.taxCode) match {
          case Some(claim) => Some(claim)
          case None        => //No Claim for the given Duty, we have to create one
            ndrcDetails.find(ndrc => ndrc.taxType === duty.taxCode) match {
              case Some(ndrc) => claimFromNdrc(ndrc)
              case None       => claimFromDuty(duty)
            }
        }
      })
      .map(_.flatten(Option.option2Iterable).toList)
  }

  private def claimFromDuty(duty: Duty): Option[Claim] =
    TaxCategory.taxCodeToCategory
      .get(duty.taxCode)
      .map(taxCategory =>
        Claim(
          "001",
          "Unknown",
          taxCategory,
          duty.taxCode,
          BigDecimal(0.0),
          BigDecimal(0.0),
          isFilled = false
        )
      )

  private def claimFromNdrc(ndrc: NdrcDetails): Option[Claim] =
    for {
      taxCode     <- TaxCode.fromString(ndrc.taxType)
      taxCategory <- TaxCategory.taxCodeToCategory.get(taxCode)
      claim       <- Claim(
                       ndrc.paymentMethod,
                       ndrc.paymentReference,
                       taxCategory,
                       taxCode,
                       BigDecimal(ndrc.amount),
                       BigDecimal(0.0),
                       isFilled = false
                     ).some
    } yield claim

  sealed trait CheckClaimAnswer extends Product with Serializable

  case object ClaimAnswersAreCorrect extends CheckClaimAnswer
  case object ClaimAnswersAreIncorrect extends CheckClaimAnswer

  implicit val checkClaimAnswerFormat: OFormat[CheckClaimAnswer] = derived.oformat[CheckClaimAnswer]()

  val summaryLanguageKey: String = "check-claim-summary"

  val checkClaimAnswerForm: Form[CheckClaimAnswer] =
    Form(
      mapping(
        summaryLanguageKey -> number
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
