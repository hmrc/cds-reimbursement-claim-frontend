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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CorrelationIdHeader
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CorrelationIdHeader._
import uk.gov.hmrc.http.HeaderNames

final case class RequestWithSessionData[A](
  sessionData: Option[SessionData],
  authenticatedRequest: AuthenticatedRequest[A]
) extends WrappedRequest[A](authenticatedRequest)
    with PreferredMessagesProvider {

  override def messagesApi: MessagesApi = authenticatedRequest.request.messagesApi

  override def headers: Headers =
    authenticatedRequest.headers
      .addIfMissing(CorrelationIdHeader.from(sessionData, authenticatedRequest.headers.get(HeaderNames.xSessionId)))
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

  override val headersFromRequestOnly: Boolean = false

  def sessionDataAction[A](
    sessionData: Option[SessionData],
    request: AuthenticatedRequest[A]
  ): RequestWithSessionData[A] =
    RequestWithSessionData(sessionData, request)

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def readHeadersFromRequestOnly(b: Boolean): SessionDataAction =
    if (b == this.headersFromRequestOnly) this
    else
      new SessionDataAction(sessionStore, errorHandler) {
        override val headersFromRequestOnly: Boolean = b
      }

}
