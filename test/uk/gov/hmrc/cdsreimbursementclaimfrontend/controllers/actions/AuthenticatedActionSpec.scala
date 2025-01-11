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

import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, ~}
import uk.gov.hmrc.auth.core.syntax.retrieved.authSyntaxForRetrieved
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{EnrolmentConfig, ErrorHandler}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.TestFeatureSwitchService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionSpec extends ControllerSpec with MockFactory with SessionSupport with AuthActionSpec {

  "AuthenticatedAction" when {

    val retrievals: Retrieval[Option[AffinityGroup] ~ Enrolments ~ Option[Credentials]] =
      Retrievals.affinityGroup and Retrievals.allEnrolments and Retrievals.credentials

    "limited access disabled" must {

      val authenticatedAction =
        new AuthenticatedAction(
          mockAuthConnector,
          config,
          instanceOf[ErrorHandler],
          mockSessionCache,
          new TestFeatureSwitchService()
        )

      def performAction[A](r: FakeRequest[A]): Future[Result] = {

        val request = new MessagesRequest[A](r, stub[MessagesApi])
        authenticatedAction.invokeBlock(
          request,
          { (a: AuthenticatedRequest[A]) =>
            a.request.messagesApi shouldBe request.messagesApi
            Future.successful(Ok)
          }
        )
      }

      "handling a not logged in user" must {

        "redirect to the login page" in {
          val requestUri = "/abc"

          List[NoActiveSession](
            BearerTokenExpired(),
            MissingBearerToken(),
            InvalidBearerToken(),
            SessionRecordNotFound()
          ).foreach { e =>
            withClue(s"For error $e: ") {
              mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

              val result = performAction(FakeRequest("GET", requestUri))
              status(result) shouldBe SEE_OTHER

              val redirectTo = redirectLocation(result)
              redirectTo shouldBe Some(
                s"$signInUrl?continue=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin"
              )
            }
          }
        }
      }

      "handling a logged in user" must {

        "effect the request action" in {
          mockAuth(EmptyPredicate, retrievals)(
            Future.successful(
              retrievedData("GB0000000001")
            )
          )

          val result = performAction(FakeRequest())
          status(result) shouldBe OK
        }

      }

      "handling the case when an authorisation exception is thrown" must {

        "throw an exception" in {
          List[AuthorisationException](
            InsufficientEnrolments(),
            UnsupportedAffinityGroup(),
            UnsupportedCredentialRole(),
            UnsupportedAuthProvider(),
            IncorrectCredentialStrength(),
            InternalError()
          ).foreach { e =>
            withClue(s"For error $e: ") {
              val exception = intercept[AuthorisationException] {
                mockAuth(EmptyPredicate, retrievals)(Future.failed(e))
                await(performAction(FakeRequest()))
              }

              exception shouldBe e
            }
          }
        }
      }
    }

    "limited access enabled" must {

      val authenticatedAction =
        new AuthenticatedAction(
          mockAuthConnector,
          config,
          instanceOf[ErrorHandler],
          mockSessionCache,
          new TestFeatureSwitchService(Feature.LimitedAccess)
        )

      def performAction[A](r: FakeRequest[A]): Future[Result] = {

        val request = new MessagesRequest[A](r, stub[MessagesApi])
        authenticatedAction.invokeBlock(
          request,
          { (a: AuthenticatedRequest[A]) =>
            a.request.messagesApi shouldBe request.messagesApi
            Future.successful(Ok)
          }
        )
      }

      "handling a not logged in user" must {

        "redirect to the login page" in {
          val requestUri = "/abc"

          List[NoActiveSession](
            BearerTokenExpired(),
            MissingBearerToken(),
            InvalidBearerToken(),
            SessionRecordNotFound()
          ).foreach { e =>
            withClue(s"For error $e: ") {
              mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

              val result = performAction(FakeRequest("GET", requestUri))
              status(result) shouldBe SEE_OTHER

              val redirectTo = redirectLocation(result)
              redirectTo shouldBe Some(
                s"$signInUrl?continue=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin"
              )
            }
          }
        }
      }

      "handling a logged in user" must {

        "effect the request action when user is on the allow list #1" in {
          mockAuth(EmptyPredicate, retrievals)(Future.successful(retrievedData("GB000000000000001")))

          val result = performAction(FakeRequest())
          status(result) shouldBe OK
        }

        "effect the request action when user is on the allow list #2" in {
          mockAuth(EmptyPredicate, retrievals)(Future.successful(retrievedData("GB000000000000002")))

          val result = performAction(FakeRequest())
          status(result) shouldBe OK
        }

        "redirect to the start page when user is NOT on the allow list" in {
          mockAuth(EmptyPredicate, retrievals)(Future.successful(retrievedData("GB000000000000003")))

          val result = performAction(FakeRequest())
          status(result)           shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some("/claim-back-import-duty-vat/unauthorised")
        }

        "handling the case when an authorisation exception is thrown" must {

          "throw an exception" in {
            List[AuthorisationException](
              InsufficientEnrolments(),
              UnsupportedAffinityGroup(),
              UnsupportedCredentialRole(),
              UnsupportedAuthProvider(),
              IncorrectCredentialStrength(),
              InternalError()
            ).foreach { e =>
              withClue(s"For error $e: ") {
                val exception = intercept[AuthorisationException] {
                  mockAuth(EmptyPredicate, retrievals)(Future.failed(e))
                  await(performAction(FakeRequest()))
                }

                exception shouldBe e
              }
            }
          }
        }

      }
    }

    def retrievedData(eori: String): Option[AffinityGroup] ~ Enrolments ~ Option[Credentials] =
      Some(Organisation) and Enrolments(
        Set(
          Enrolment(
            EoriEnrolment.key,
            Seq(
              EnrolmentIdentifier(
                EoriEnrolment.eoriEnrolmentIdentifier,
                eori
              )
            ),
            ""
          )
        )
      ) and Some(Credentials("gg-cred-id", "GovernmentGateway"))

  }

}
