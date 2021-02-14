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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UKDutyAmountAnswers.IncompleteUKDutyAmountAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData, UKDutyAmountAnswers, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BooleanFormatter

import scala.concurrent.Future

@Singleton
class EnterDutyAmountsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterPaidAndClaimAmountsPage: pages.enter_paid_and_claim_amounts
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withDutyAmountAnswers(
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

  def enterUkDutyAmounts: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDutyAmountAnswers { (_, _, answers) =>
        answers.fold(
          _ => Ok(enterPaidAndClaimAmountsPage(EnterDutyAmountsController.enterClaimForm)),
          _ => Ok(enterPaidAndClaimAmountsPage(EnterDutyAmountsController.enterClaimForm))
        )
      }
    }

  def enterUkDutyAmountsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDutyAmountAnswers { (_, _, _) =>
        EnterDutyAmountsController.enterClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors => {
              println(requestFormWithErrors.toString)
              BadRequest(
                enterPaidAndClaimAmountsPage(
                  requestFormWithErrors
                )
              )
            },
            _ => Ok("good result")
          )
      }
    }

}

object EnterDutyAmountsController {

  final case class EnterClaim(
    dutyAmounts: List[DutyAmount],
    makeEuDutyClaim: Boolean
  )

  object EnterClaim {
    implicit val format: OFormat[EnterClaim] = Json.format[EnterClaim]
  }

  final case class DutyAmount(
    paid: Option[BigDecimal],
    claim: Option[BigDecimal]
  )

  object DutyAmount {
    implicit val format: OFormat[DutyAmount] = Json.format[DutyAmount]
  }

  val enterClaimForm: Form[EnterClaim] = Form(
    mapping(
      "enter-duty-and-claim-amounts"                    -> list(
        mapping(
          "paid"  -> optional(bigDecimal),
          "claim" -> optional(bigDecimal)
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
      case (None, None)              => true
      case _                         => false
    }

  def isValidClaim(enterClaim: EnterClaim): Boolean =
    if (enterClaim.makeEuDutyClaim === false) {
      //means they do not want to add any eu tax
      if (enterClaim.dutyAmounts.nonEmpty) {
        // now make sure there are valid pairs
        enterClaim.dutyAmounts.forall(p => isValidPaidClaimEntry(p))
      } else
        false
    } else {
      // just ensure that there valid pairs if provided
      if (enterClaim.dutyAmounts.nonEmpty) {
        if (enterClaim.dutyAmounts.forall(p => p.paid.isEmpty && p.claim.isEmpty)) {
          true
        } else {
          enterClaim.dutyAmounts.forall(p => isValidPaidClaimEntry(p))
        }
      } else {
        true
      }
    }

}
