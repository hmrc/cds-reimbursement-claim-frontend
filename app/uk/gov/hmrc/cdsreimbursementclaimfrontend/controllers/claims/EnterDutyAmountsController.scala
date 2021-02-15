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
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDutyAmountsController.{removeZeroClaims, removeZeroEuClaims}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EuDutyAmountAnswers.IncompleteEuDutyAmountAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.{EUTaxCode, UKTaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UKDutyAmountAnswers.IncompleteUKDutyAmountAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BooleanFormatter, DraftClaim, Error, EuDutyAmountAnswers, SessionData, TaxCode, UKDutyAmountAnswers, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDutyAmountsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterUkPaidAndClaimAmountsPage: pages.enter_uk_paid_and_claim_amounts,
  enterEuPaidAndClaimAmountsPage: pages.enter_eu_paid_and_claim_amounts
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withUkDutyAmountAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      UKDutyAmountAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimantDetailsAsIndividualAnswer = draftClaim.fold(
          _.ukDutyAmountAnswers
        )
        maybeClaimantDetailsAsIndividualAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteUKDutyAmountAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  private def withEuDutyAmountAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      EuDutyAmountAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimantDetailsAsIndividualAnswer = draftClaim.fold(
          _.euDutyAmountAnswers
        )
        maybeClaimantDetailsAsIndividualAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteEuDutyAmountAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterUkDutyAmounts: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUkDutyAmountAnswers { (_, _, answers) =>
        answers.fold(
          _ => Ok(enterUkPaidAndClaimAmountsPage(EnterDutyAmountsController.enterClaimForm)),
          _ => Ok(enterUkPaidAndClaimAmountsPage(EnterDutyAmountsController.enterClaimForm))
        )
      }
    }

  def enterUkDutyAmountsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUkDutyAmountAnswers { (_, fillingOutClaim, answers) =>
        EnterDutyAmountsController.enterClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterUkPaidAndClaimAmountsPage(
                  requestFormWithErrors
                )
              ),
            enterClaim => {
              val updatedAnswers = answers.fold(
                incomplete =>
                  incomplete.copy(
                    ukDutyAmounts = Some(removeZeroClaims(enterClaim))
                  ),
                complete => complete.copy(ukDutyAmounts = removeZeroClaims(enterClaim))
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(ukDutyAmountAnswers = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not get uk duty details", e)
                  errorHandler.errorResult()
                },
                _ =>
                  if (enterClaim.makeEuDutyClaim) Redirect(routes.EnterDutyAmountsController.enterEuDutyAmounts())
                  else Redirect(routes.CheckReimbursementClaimTotalController.checkReimbursementClaimTotal())
              )
            }
          )
      }
    }

  def enterEuDutyAmounts: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withEuDutyAmountAnswers { (_, _, answers) =>
        answers.fold(
          _ => Ok(enterEuPaidAndClaimAmountsPage(EnterDutyAmountsController.enterEuClaimForm)),
          _ => Ok(enterEuPaidAndClaimAmountsPage(EnterDutyAmountsController.enterEuClaimForm))
        )
      }
    }

  def enterEuDutyAmountsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withEuDutyAmountAnswers { (_, fillingOutClaim, answers) =>
        EnterDutyAmountsController.enterEuClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterEuPaidAndClaimAmountsPage(
                  requestFormWithErrors
                )
              ),
            enterClaim => {
              val updatedAnswers = answers.fold(
                incomplete =>
                  incomplete.copy(
                    euDutyAmounts = Some(removeZeroEuClaims(enterClaim))
                  ),
                complete => complete.copy(euDutyAmounts = removeZeroEuClaims(enterClaim))
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(euDutyAmountAnswers = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture eu duty details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckReimbursementClaimTotalController.checkReimbursementClaimTotal())
              )
            }
          )
      }
    }

}

object EnterDutyAmountsController {

  def removeZeroEuClaims(enterEuClaim: EnterEuClaim): EnterEuClaim =
    EnterEuClaim(enterEuClaim.dutyAmounts.filterNot(p => p.claim === Some(BigDecimal(0)) || p.claim.isEmpty))

  def removeZeroClaims(enterClaim: EnterClaim): EnterClaim =
    EnterClaim(
      enterClaim.dutyAmounts.filterNot(p => p.claim === Some(BigDecimal(0)) || p.claim.isEmpty),
      enterClaim.makeEuDutyClaim
    )

  final case class EnterEuClaim(
    dutyAmounts: List[EuDutyAmount]
  )

  object EnterEuClaim {
    implicit val format: OFormat[EnterEuClaim] = Json.format[EnterEuClaim]
  }

  final case class EnterClaim(
    dutyAmounts: List[DutyAmount],
    makeEuDutyClaim: Boolean
  )

  object EnterClaim {
    implicit val format: OFormat[EnterClaim] = Json.format[EnterClaim]
  }

  final case class DutyAmount(
    taxCode: UKTaxCode,
    paid: Option[BigDecimal],
    claim: Option[BigDecimal]
  )

  object DutyAmount {
    implicit val format: OFormat[DutyAmount] = Json.format[DutyAmount]
  }

  final case class EuDutyAmount(
    taxCode: EUTaxCode,
    paid: Option[BigDecimal],
    claim: Option[BigDecimal]
  )

  object EuDutyAmount {
    implicit val format: OFormat[EuDutyAmount] = Json.format[EuDutyAmount]
  }

  val enterEuClaimForm: Form[EnterEuClaim] = Form(
    mapping(
      "enter-duty-and-claim-amounts-eu" -> list(
        mapping(
          "tax-code" -> number
            .verifying(
              "invalid eu tax code",
              code =>
                code === 0 ||
                  code === 1 ||
                  code === 2 ||
                  code === 3 ||
                  code === 4 ||
                  code === 5 ||
                  code === 6
            )
            .transform[EUTaxCode](
              {
                case 0 => TaxCode.EUTaxCode.A50
                case 1 => TaxCode.EUTaxCode.A70
                case 2 => TaxCode.EUTaxCode.A80
                case 3 => TaxCode.EUTaxCode.A85
                case 4 => TaxCode.EUTaxCode.A90
                case 5 => TaxCode.EUTaxCode.A95
                case 6 => TaxCode.EUTaxCode.B05
              },
              {
                case TaxCode.EUTaxCode.A50 => 0
                case TaxCode.EUTaxCode.A70 => 1
                case TaxCode.EUTaxCode.A80 => 2
                case TaxCode.EUTaxCode.A85 => 3
                case TaxCode.EUTaxCode.A90 => 4
                case TaxCode.EUTaxCode.A95 => 5
                case TaxCode.EUTaxCode.B05 => 6
              }
            ),
          "paid"     -> optional(bigDecimal),
          "claim"    -> optional(bigDecimal)
        )(EuDutyAmount.apply)(EuDutyAmount.unapply)
      )
    )(EnterEuClaim.apply)(EnterEuClaim.unapply)
      .verifying("enter-duty-and-claim-amounts-eu.invalid.claim", claim => isValidEuClaim(claim))
  )

  val enterClaimForm: Form[EnterClaim] = Form(
    mapping(
      "enter-duty-and-claim-amounts"                    -> list(
        mapping(
          "tax-code" -> number
            .verifying(
              "invalid uk tax code",
              code =>
                code === 0 ||
                  code === 1 ||
                  code === 2 ||
                  code === 3 ||
                  code === 4 ||
                  code === 5 ||
                  code === 6
            )
            .transform[UKTaxCode](
              {
                case 0 => TaxCode.UKTaxCode.A00
                case 1 => TaxCode.UKTaxCode.A20
                case 2 => TaxCode.UKTaxCode.A30
                case 3 => TaxCode.UKTaxCode.A35
                case 4 => TaxCode.UKTaxCode.A40
                case 5 => TaxCode.UKTaxCode.A45
                case 6 => TaxCode.UKTaxCode.B00
              },
              {
                case TaxCode.UKTaxCode.A00 => 0
                case TaxCode.UKTaxCode.A20 => 1
                case TaxCode.UKTaxCode.A30 => 2
                case TaxCode.UKTaxCode.A35 => 3
                case TaxCode.UKTaxCode.A40 => 4
                case TaxCode.UKTaxCode.A45 => 5
                case TaxCode.UKTaxCode.B00 => 6
              }
            ),
          "paid"     -> optional(bigDecimal),
          "claim"    -> optional(bigDecimal)
        )(DutyAmount.apply)(DutyAmount.unapply)
      ),
      "enter-duty-and-claim-amounts.make-eu-duty-claim" -> of(BooleanFormatter.formatter)
    )(EnterClaim.apply)(EnterClaim.unapply)
      .verifying("enter-duty-and-claim-amounts.invalid.claim", claim => isValidClaim(claim))
  )

  //TODO: add complete validation (negative values etc)
  def isValidPaidClaimEntry(dutyAmount: DutyAmount): Boolean =
    (dutyAmount.paid, dutyAmount.claim) match {
      case (Some(paid), Some(claim)) => claim <= paid
      case _                         => false
    }

  def isValidClaim(enterClaim: EnterClaim): Boolean =
    if (enterClaim.makeEuDutyClaim === false) {
      //means they do not want to add any eu tax
      if (enterClaim.dutyAmounts.nonEmpty) {
        // now make sure there are valid pairs
        val f = enterClaim.dutyAmounts.filterNot(p => p.paid.isEmpty && p.claim.isEmpty)
        if (f.nonEmpty) {
          f.forall(p => isValidPaidClaimEntry(p))
        } else {
          false
        }
      } else
        false
    } else {
      // just ensure that there valid pairs if provided
      if (enterClaim.dutyAmounts.nonEmpty) {
        val f = enterClaim.dutyAmounts.filterNot(p => p.paid.isEmpty && p.claim.isEmpty)
        if (f.forall(p => p.paid.isEmpty && p.claim.isEmpty)) {
          true
        } else {
          f.forall(p => isValidPaidClaimEntry(p))
        }
      } else {
        true
      }
    }

  //TODO: add complete validation (negative values etc)
  def isValidEuPaidClaimEntry(dutyAmount: EuDutyAmount): Boolean =
    (dutyAmount.paid, dutyAmount.claim) match {
      case (Some(paid), Some(claim)) => claim <= paid
      case (None, None)              => true
      case _                         => false
    }

  def isValidEuClaim(enterEuClaim: EnterEuClaim): Boolean =
    // just ensure that there valid pairs if provided
    if (enterEuClaim.dutyAmounts.nonEmpty) {
      if (enterEuClaim.dutyAmounts.forall(p => p.paid.isEmpty && p.claim.isEmpty)) {
        true
      } else {
        enterEuClaim.dutyAmounts.forall(p => isValidEuPaidClaimEntry(p))
      }
    } else {
      true
    }

}
