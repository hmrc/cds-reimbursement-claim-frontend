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
import play.api.mvc.*
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CorrelationIdHeader
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CorrelationIdHeader.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HeaderNames
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.play.http.HeaderCarrierConverter

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

  protected val headersFromRequestOnly: Boolean = false

  override def authorisedFunction[A](
    auth: AuthorisedFunctions,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequest[A]]] = {

    implicit val hc: HeaderCarrier =
      if headersFromRequestOnly then HeaderCarrierConverter.fromRequest(request)
      else
        HeaderCarrierConverter
          .fromRequestAndSession(request, request.session)

    auth
      .authorised()
      .retrieve(
        Retrievals.affinityGroup and
          Retrievals.allEnrolments and
          Retrievals.credentials
      ) { case affinityGroup ~ enrolments ~ creds =>
        withGGCredentials(creds, request) {
          affinityGroup match {
            case Some(AffinityGroup.Individual) | Some(AffinityGroup.Organisation) =>
              handleUser(
                enrolments,
                request
              )

            case other =>
              logger.warn(s"User has unsupported affinity group type $other")
              Future.successful(Left(errorHandler.errorResult()(request)))
          }
        }

      }
  }

  private def handleUser[A](
    enrolments: Enrolments,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequest[A]]] =
    hasEoriEnrolment(enrolments) map {
      case Left(_)           => Left(Redirect(routes.UnauthorisedController.unauthorised()))
      case Right(Some(eori)) =>
        Right(
          AuthenticatedRequest(
            request,
            Some(eori)
          )
        )
      case Right(None)       =>
        Left(Redirect(unauthorizedErrorPage))
    }

  private def hasEoriEnrolment[A](
    enrolments: Enrolments
  ): Future[Either[Result, Option[Eori]]] =
    enrolments.getEnrolment(EoriEnrolment.key) match {
      case Some(eori) =>
        eori.getIdentifier(EoriEnrolment.eoriEnrolmentIdentifier) match {
          case Some(eori) =>
            Future.successful(Right(Some(Eori(eori.value))))
          case None       =>
            logger.warn("EORI is missing from the enrolment")
            Future.successful(Left(Redirect(unauthorizedErrorPage)))
        }
      case None       =>
        logger.warn("No EORI enrolment")
        Future.successful(Left(Redirect(unauthorizedErrorPage)))
    }

  private def withGGCredentials[A](
    credentials: Option[Credentials],
    request: MessagesRequest[A]
  )(
    f: => Future[
      Either[Result, AuthenticatedRequest[A]]
    ]
  ): Future[Either[Result, AuthenticatedRequest[A]]] =
    credentials match {
      case None =>
        logger.warn("No credentials were retrieved")
        Future.successful(Left(errorHandler.errorResult()(request)))

      case Some(Credentials(_, "GovernmentGateway")) =>
        f

      case Some(Credentials(_, _)) =>
        Future.successful(
          Right(
            AuthenticatedRequest(
              request,
              None
            )
          )
        )
    }

  def readHeadersFromRequestOnly(isCallback: Boolean): AuthenticatedAction =
    if isCallback
    then
      new AuthenticatedAction(
        authConnector,
        config,
        errorHandler,
        sessionStore,
        featureSwitchService
      ) {
        protected override val headersFromRequestOnly: Boolean = true
      }
    else this

}
