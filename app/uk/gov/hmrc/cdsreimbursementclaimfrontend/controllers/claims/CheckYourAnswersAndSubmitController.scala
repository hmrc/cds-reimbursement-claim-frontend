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

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, RetrievedUserType, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class CheckYourAnswersAndSubmitController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  checkYourAnswersPage: pages.check_your_answers,
  confirmationOfSubmissionPage: pages.confirmation_of_submission,
  submitClaimFailedPage: pages.submit_claim_error
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def checkAllAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(checkYourAnswersPage())
    }

  def checkAllAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData {
      Ok("implement submission")
    }

  def submissionError(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubmitClaimFailedOrSubscribed(request)(_ => Ok(submitClaimFailedPage()))
    }

  def confirmationOfSubmission(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withJustSubmittedClaim(request)(j => Ok(confirmationOfSubmissionPage(j)))
    }

  private def withJustSubmittedClaim(
    request: RequestWithSessionData[_]
  )(f: JustSubmittedClaim => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(j: JustSubmittedClaim) => f(j)
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withSubmitClaimFailedOrSubscribed(
    request: RequestWithSessionData[_]
  )(
    f: Either[SubmitClaimFailed, RetrievedUserType] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubmitClaimFailed) => f(Left(s))
      // case Some(s: SignedInUser)      => f(Right(s))
      case _                          => Redirect(baseRoutes.StartController.start())
    }

}

object CheckYourAnswersAndSubmitController {

  sealed trait SubmitClaimResult

  object SubmitClaimResult {

    final case class SubmitClaimError(error: Error) extends SubmitClaimResult

    final case class SubmitClaimSuccess(response: SubmitClaimResponse) extends SubmitClaimResult

  }

}
