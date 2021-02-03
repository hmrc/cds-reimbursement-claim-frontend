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

import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserType.NonGovernmentGatewayUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, RetrievedUserType, UserType}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedData[A](
  journeyUserType: RetrievedUserType,
  userType: Option[UserType],
  request: MessagesRequest[A]
) extends WrappedRequest[A](request)

@Singleton
class AuthenticatedActionWithRetrievedData @Inject() (
  val authConnector: AuthConnector,
  val config: Configuration,
  val errorHandler: ErrorHandler,
  val sessionStore: SessionCache
)(implicit val executionContext: ExecutionContext)
    extends AuthenticatedActionBase[AuthenticatedRequestWithRetrievedData] {

  override def authorisedFunction[A](
    auth: AuthorisedFunctions,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] = {

    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter
        .fromHeadersAndSession(request.headers, Some(request.session))

    auth
      .authorised()
      .retrieve(
        Retrievals.affinityGroup and
          Retrievals.email and
          Retrievals.allEnrolments and
          Retrievals.credentials
      ) { case affinityGroup ~ maybeEmail ~ enrolments ~ creds =>
        withGGCredentials(creds, request) { ggCredId =>
          affinityGroup match {

            case Some(AffinityGroup.Individual) =>
              handleIndividualOrOrganisation(
                Right(AffinityGroup.Individual),
                maybeEmail,
                enrolments,
                ggCredId,
                request
              )

            case Some(AffinityGroup.Organisation) =>
              handleIndividualOrOrganisation(
                Left(AffinityGroup.Organisation),
                None,
                enrolments,
                ggCredId,
                request
              )

            case other =>
              logger.warn(s"User has unsupported affinity group type $other")
              Future.successful(Left(errorHandler.errorResult(None)(request)))
          }
        }
      }
  }

  private def handleIndividualOrOrganisation[A](
    affinityGroup: Either[
      Organisation,
      Individual
    ],
    maybeEmail: Option[String],
    enrolments: Enrolments,
    ggCredId: GGCredId,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] =
    hasEoriEnrolment(enrolments, request) map {
      case Left(errorResult) => Left(errorResult)
      case Right(Some(eori)) =>
        handleSignedInUser(eori, ggCredId, affinityGroup, maybeEmail, request)
      case Right(None)       =>
        Left(errorHandler.errorResult(None)(request))
    }

  private def hasEoriEnrolment[A](
    enrolments: Enrolments,
    request: MessagesRequest[A]
  ): Future[Either[Result, Option[Eori]]] =
    enrolments.getEnrolment(EoriEnrolment.key) match {
      case Some(eori) =>
        eori.getIdentifier(EoriEnrolment.eoriEnrolmentIdentifier) match {
          case Some(eori) =>
            Future.successful(Right(Some(Eori(eori.value))))
          case None       =>
            logger.warn(s"EORI is missing from the enrolment")
            Future.successful(Left(errorHandler.errorResult(None)(request)))
        }
      case None       =>
        logger.warn("No EORI enrolment")
        Future.successful(Left(errorHandler.errorResult(None)(request)))
    }

  private def handleSignedInUser[A](
    eori: Eori,
    ggCredId: GGCredId,
    affinityGroup: Either[
      Organisation,
      Individual
    ],
    maybeEmail: Option[String],
    request: MessagesRequest[A]
  ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] = {
    def authenticatedRequest(userType: UserType): AuthenticatedRequestWithRetrievedData[A] =
      if (userType === UserType.Individual) {
        AuthenticatedRequestWithRetrievedData(
          RetrievedUserType.Individual(ggCredId, maybeEmail.map(Email(_)), eori),
          Some(userType),
          request
        )

      } else {
        AuthenticatedRequestWithRetrievedData(
          RetrievedUserType.Organisation(ggCredId, eori),
          Some(userType),
          request
        )
      }

    affinityGroup match {
      case Right(AffinityGroup.Individual) =>
        Right(authenticatedRequest(UserType.Individual))

      case Left(AffinityGroup.Organisation) =>
        Right(authenticatedRequest(UserType.Organisation))
    }
  }

  private def withGGCredentials[A](
    credentials: Option[Credentials],
    request: MessagesRequest[A]
  )(
    f: GGCredId => Future[
      Either[Result, AuthenticatedRequestWithRetrievedData[A]]
    ]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] =
    credentials match {
      case None =>
        logger.warn("No credentials were retrieved")
        Future.successful(Left(errorHandler.errorResult(None)(request)))

      case Some(Credentials(id, "GovernmentGateway")) =>
        f(GGCredId(id))

      case Some(Credentials(_, otherProvider)) =>
        Future.successful(
          Right(
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.NonGovernmentGatewayRetrievedUser(
                otherProvider
              ),
              Some(NonGovernmentGatewayUser),
              request
            )
          )
        )
    }
}
