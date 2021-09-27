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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import com.google.inject.{Inject, Singleton}
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Redirect
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{JourneyStatus, SessionData, SignedInUserDetails, UserType}

import scala.concurrent.{ExecutionContext, Future}

final case class RequestWithSessionData[A](
  sessionData: Option[SessionData],
  authenticatedRequest: AuthenticatedRequest[A]
) extends WrappedRequest[A](authenticatedRequest)
    with PreferredMessagesProvider {

  override def messagesApi: MessagesApi = authenticatedRequest.request.messagesApi

  val userType: Option[UserType] = sessionData.flatMap(_.userType)

  val signedInUserDetails: Option[SignedInUserDetails] = sessionData.flatMap(_.journeyStatus).collect {
    case JourneyStatus.FillingOutClaim(_, signedInUserDetails, _)          => signedInUserDetails
    case JourneyStatus.JustSubmittedClaim(_, signedInUserDetails, _, _, _) => signedInUserDetails
    case JourneyStatus.SubmitClaimFailed(_, signedInUserDetails, _)        => signedInUserDetails
  }

  def using(
    matchExpression: PartialFunction[JourneyStatus, Future[Result]],
    applyIfNone: => Result = startNewJourney
  ): Future[Result] =
    sessionData
      .flatMap(session => session.journeyStatus)
      .collect(matchExpression)
      .getOrElse(Future.successful(applyIfNone))

  def startNewJourney: Result =
    Redirect(baseRoutes.StartController.start())
}

@Singleton
class SessionDataAction @Inject() (
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler
)(implicit
  val executionContext: ExecutionContext
) extends SessionDataActionBase[
      AuthenticatedRequest,
      RequestWithSessionData
    ] {

  def sessionDataAction[A](
    sessionData: Option[SessionData],
    request: AuthenticatedRequest[A]
  ): RequestWithSessionData[A] =
    RequestWithSessionData(sessionData, request)

}
