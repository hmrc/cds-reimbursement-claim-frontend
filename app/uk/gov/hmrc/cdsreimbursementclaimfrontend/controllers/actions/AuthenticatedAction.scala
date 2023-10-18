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
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CorrelationIdHeader
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CorrelationIdHeader._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.http.HeaderNames
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final case class AuthenticatedRequest[A](request: MessagesRequest[A], maybeEori: Option[Eori] = None)
    extends WrappedRequest[A](request) {

  override def headers: Headers =
    request.headers
      .addIfMissing(
        CorrelationIdHeader.from(
          maybeEori,
          request.session
            .get(SessionKeys.sessionId)
            .orElse(request.headers.get(HeaderNames.xSessionId))
        )
      )
}

@Singleton
class AuthenticatedAction @Inject() (
  val authConnector: AuthConnector,
  val config: Configuration,
  val errorHandler: ErrorHandler,
  val sessionStore: SessionCache,
  featureSwitchService: FeatureSwitchService
)(implicit val executionContext: ExecutionContext)
    extends AuthenticatedActionBase[AuthenticatedRequest] {

  val headersFromRequestOnly: Boolean = false

  override def authorisedFunction[A](
    auth: AuthorisedFunctions,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequest[A]]] = {
    implicit val hc: HeaderCarrier =
      if (headersFromRequestOnly)
        HeaderCarrierConverter.fromRequest(request)
      else
        HeaderCarrierConverter
          .fromRequestAndSession(request, request.session)

    if (featureSwitchService.isDisabled(Feature.LimitedAccess))
      auth
        .authorised()
        .retrieve(
          Retrievals.allEnrolments
        ) { case enrolments =>
          Future.successful(Right(AuthenticatedRequest(request, getEori(enrolments))))
        }
    else
      auth
        .authorised()
        .retrieve(
          Retrievals.allEnrolments
        ) { case enrolments =>
          if (checkUserHasAccess(enrolments))
            Future.successful(Right(AuthenticatedRequest(request, getEori(enrolments))))
          else
            Future.successful(Left(Results.Redirect(limitedAccessErrorPage)))
        }
  }

  def getEori(enrolments: Enrolments): Option[Eori] =
    enrolments.getEnrolment(EoriEnrolment.key) match {
      case Some(eori) =>
        eori.getIdentifier(EoriEnrolment.eoriEnrolmentIdentifier) match {
          case Some(eori) => Some(Eori(eori.value))
          case None       => None
        }
      case None       => None
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  final def readHeadersFromRequestOnly(b: Boolean): AuthenticatedAction =
    if (b == headersFromRequestOnly) this
    else
      new AuthenticatedAction(
        authConnector,
        config,
        errorHandler,
        sessionStore,
        featureSwitchService
      ) {
        override val headersFromRequestOnly: Boolean = b
      }

}

object AuthenticatedAction {
  final case class OnlyRequest(val value: Boolean) extends AnyVal
}
