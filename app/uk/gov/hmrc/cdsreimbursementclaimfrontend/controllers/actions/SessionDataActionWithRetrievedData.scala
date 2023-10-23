/*
 * Copyright 2023 HM Revenue & Customs
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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AuthenticatedUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final case class RequestWithSessionDataAndRetrievedData[A](
  sessionData: SessionData,
  authenticatedRequest: AuthenticatedRequestWithRetrievedData[A]
) extends WrappedRequest[A](authenticatedRequest)
    with PreferredMessagesProvider {

  override def messagesApi: MessagesApi =
    authenticatedRequest.request.messagesApi

  def whenAuthorisedUser(f: (Eori, Option[Name]) => Future[Result])(resultIfUnsupportedUser: => Result)(implicit
    request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    request.authenticatedRequest.journeyUserType match {
      case AuthenticatedUser.NonGovernmentGatewayAuthenticatedUser(_) =>
        Future.successful(resultIfUnsupportedUser)

      case AuthenticatedUser.Organisation(_, eori, name) =>
        f(eori, name)

      case AuthenticatedUser.Individual(_, eori, name) =>
        f(eori, name)
    }

}

@Singleton
class SessionDataActionWithRetrievedData @Inject() (
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler
)(implicit
  val executionContext: ExecutionContext
) extends SessionDataActionBase[
      AuthenticatedRequestWithRetrievedData,
      RequestWithSessionDataAndRetrievedData
    ] {

  override val headersFromRequestOnly: Boolean = false

  def sessionDataAction[A](
    sessionData: Option[SessionData],
    request: AuthenticatedRequestWithRetrievedData[A]
  ): RequestWithSessionDataAndRetrievedData[A] = {
    val data: SessionData =
      sessionData.getOrElse(SessionData.empty)
    RequestWithSessionDataAndRetrievedData(data, request)
  }

}
