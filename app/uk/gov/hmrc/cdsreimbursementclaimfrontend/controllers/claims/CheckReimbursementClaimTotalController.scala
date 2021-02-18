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
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimAnswers.CompleteClaimAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EuDutyAmountAnswers.IncompleteEuDutyAmountAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UKDutyAmountAnswers.IncompleteUKDutyAmountAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, DraftClaim, Error, SessionData, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckReimbursementClaimTotalController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  checkReimbursementClaimTotalPage: pages.check_reimbursement_claim_total
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def isClaimZero(claim: Option[BigDecimal]): Boolean =
    claim match {
      case Some(value) => value =!= BigDecimal(0)
      case None        => true
    }

  private def withReimbursementClaimTotals(
    f: (
      SessionData,
      FillingOutClaim,
      List[Claim]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeUkDutyAnswers  = draftClaim.fold(
          _.ukDutyAmountAnswers
        )
        val maybeEuDutyAnswers  = draftClaim.fold(
          _.euDutyAmountAnswers
        )
        val claims: List[Claim] = maybeEuDutyAnswers
          .getOrElse(IncompleteEuDutyAmountAnswer.empty)
          .dutyAmounts
          .filter(d => isClaimZero(d.claim))
          .map(p => Claim(p.taxCode, p.paid.getOrElse(BigDecimal(0)), p.claim.getOrElse(BigDecimal(0)))) ++
          maybeUkDutyAnswers
            .getOrElse(IncompleteUKDutyAmountAnswer.empty)
            .dutyAmounts
            .filter(d => isClaimZero(d.claim))
            .map(p => Claim(p.taxCode, p.paid.getOrElse(BigDecimal(0)), p.claim.getOrElse(BigDecimal(0))))

        f(sessionData, fillingOutClaim, claims)
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def checkReimbursementClaimTotal: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withReimbursementClaimTotals { (_, _, answers) =>
        Ok(checkReimbursementClaimTotalPage(answers))
      }
    }

  def checkReimbursementClaimTotalSubmit: Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withReimbursementClaimTotals { (_, fillingOutClaim, answers) =>
        val updatedAnswers = CompleteClaimAnswers(answers)

        val newDraftClaim = fillingOutClaim.draftClaim.fold(_.copy(claimAnswers = Some(updatedAnswers)))

        val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = EitherT
          .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
          .leftMap((_: Unit) => Error("could not update session"))

        result.fold(
          e => {
            logger.warn("could not claim amounts", e)
            errorHandler.errorResult()
          },
          _ => Redirect(routes.BankAccountController.enterBankAccountDetails())
        )
      }
  }
}

object CheckReimbursementClaimTotalController {}
