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
import play.api.libs.json.Json
import play.api.mvc.Results.Ok
import play.api.mvc.MessagesRequest
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.auth.core.syntax.retrieved.authSyntaxForRetrieved
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.RetrievalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AuthenticatedUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.TestFeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionWithRetrievedDataSpec
    extends ControllerSpec
    with MockFactory
    with SessionSupport
    with AuthActionSpec {

  val retrievals: Retrieval[Option[AffinityGroup] ~ Enrolments ~ Option[Credentials]] =
    Retrievals.affinityGroup and
      Retrievals.allEnrolments and
      Retrievals.credentials

  val emptyEnrolments: Enrolments = Enrolments(Set.empty)

  val mockEoriDetailsConnector: EoriDetailsConnector = mock[EoriDetailsConnector]

  def eoriEnrolment(eori: String): Enrolments = Enrolments(
    Set(
      Enrolment(
        "HMRC-CUS-ORG",
        Seq(
          EnrolmentIdentifier(
            "EORINumber",
            eori
          )
        ),
        "Activated",
        None
      )
    )
  )

  val someOtherEnrolment: Enrolments = Enrolments(
    Set(
      Enrolment(
        "HMRC-VAT-ORG",
        Seq(
          EnrolmentIdentifier(
            "VAT Number",
            "some vat number"
          )
        ),
        "Activated",
        None
      )
    )
  )

  def mockGetEoriDetails(eori: Eori, name: String = "John Smith") =
    (mockEoriDetailsConnector
      .getCurrentUserEoriDetails(_: HeaderCarrier))
      .expects(*)
      .returning(
        Future.successful(
          Some(
            EoriDetailsConnector
              .Response(
                eoriGB = eori,
                eoriXI = None,
                fullName = name,
                eoriEndDate = None
              )
          )
        )
      )

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  val ggCredentials = Credentials("id", "GovernmentGateway")

  "Authenticated action with retrieved data" when {

    "limited access disabled" must {

      val authenticatedAction =
        new AuthenticatedActionWithRetrievedData(
          mockAuthConnector,
          config,
          instanceOf[ErrorHandler],
          mockSessionCache,
          new TestFeatureSwitchService(),
          mockEoriDetailsConnector
        )

      def performAction[A](r: FakeRequest[A]): Future[Result] = {
        @SuppressWarnings(Array("org.wartremover.warts.Any"))
        val request = new MessagesRequest[A](r, stub[MessagesApi])
        authenticatedAction.invokeBlock(
          request,
          { a: AuthenticatedRequestWithRetrievedData[A] =>
            a.request.messagesApi shouldBe request.messagesApi
            Future.successful(Ok(Json.toJson(a.journeyUserType)))
          }
        )
      }

      "handling a user who has logged in with an auth provider which isn't gg" must {

        "return the auth provider id" in {
          val providerType     = "other provider"
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Organisation) and emptyEnrolments and Some(
              Credentials("id", providerType)
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result = performAction(FakeRequest())

          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson[AuthenticatedUser.NonGovernmentGatewayAuthenticatedUser](
            AuthenticatedUser.NonGovernmentGatewayAuthenticatedUser(providerType)
          )
        }

      }

      "handling a logged in user with an eori enrolment" must {

        val eori = sample[Eori]

        "return the signed in details for an individual" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Individual) and eoriEnrolment(
              eori.value
            ) and Some(
              ggCredentials
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockGetEoriDetails(eori)

          val result = performAction(FakeRequest())

          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson[AuthenticatedUser.Individual](
            AuthenticatedUser
              .Individual(
                None,
                eori,
                Some("John Smith")
              )
          )
        }

        "return the signed in details for an organisation" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Organisation) and eoriEnrolment(
              eori.value
            ) and Some(
              ggCredentials
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockGetEoriDetails(eori)

          val result = performAction(FakeRequest())

          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson[AuthenticatedUser.Organisation](
            AuthenticatedUser
              .Organisation(
                None,
                eori,
                Some("John Smith")
              )
          )
        }
      }

      "handling a logged in user with no eori enrolment" must {

        "redirect to unauthorised page" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Individual) and emptyEnrolments and Some(
              ggCredentials
            ))

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          }

          val result = performAction(FakeRequest())

          val redirectTo = redirectLocation(result)
          redirectTo shouldBe Some(
            routes.UnauthorisedController.unauthorised().url
          )
        }
      }

      "handling cases with incorrect type of credentials" must {

        "redirect to unauthorised page" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Individual) and someOtherEnrolment and Some(
              ggCredentials
            ))

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          }

          val result = performAction(FakeRequest())

          val redirectTo = redirectLocation(result)
          redirectTo shouldBe Some(
            routes.UnauthorisedController.unauthorised().url
          )
        }
      }

      "handling an unauthenticated user" must {

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

      "handling the case when an authorisation exception is thrown" must {

        "throw an exception" in {
          List[AuthorisationException](
            InsufficientEnrolments(),
            UnsupportedAffinityGroup(),
            UnsupportedAuthProvider(),
            UnsupportedCredentialRole(),
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
        new AuthenticatedActionWithRetrievedData(
          mockAuthConnector,
          config,
          instanceOf[ErrorHandler],
          mockSessionCache,
          new TestFeatureSwitchService(Feature.LimitedAccess),
          mockEoriDetailsConnector
        )

      def performAction[A](r: FakeRequest[A]): Future[Result] = {
        @SuppressWarnings(Array("org.wartremover.warts.Any"))
        val request = new MessagesRequest[A](r, stub[MessagesApi])
        authenticatedAction.invokeBlock(
          request,
          { a: AuthenticatedRequestWithRetrievedData[A] =>
            a.request.messagesApi shouldBe request.messagesApi
            Future.successful(Ok(Json.toJson(a.journeyUserType)))
          }
        )
      }

      "handling a user who has logged in with an auth provider which isn't gg" must {

        "redirect to the start page" in {
          val providerType     = "other provider"
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Organisation) and emptyEnrolments and Some(
              Credentials("id", providerType)
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result = performAction(FakeRequest())

          status(result) shouldBe SEE_OTHER
        }
      }

      "handling a logged in user with an eori enrolment" must {

        "return the signed in details for an individual" in {
          val eoriNumber       = "GB000000000000001"
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Individual) and eoriEnrolment(
              eoriNumber
            ) and Some(
              ggCredentials
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockGetEoriDetails(Eori(eoriNumber))

          val result = performAction(FakeRequest())

          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson[AuthenticatedUser.Individual](
            AuthenticatedUser
              .Individual(
                None,
                Eori("GB000000000000001"),
                Some("John Smith")
              )
          )
        }

        "return the signed in details for an organisation" in {
          val eoriNumber       = "GB000000000000002"
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Organisation) and eoriEnrolment(
              eoriNumber
            ) and Some(
              ggCredentials
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockGetEoriDetails(Eori(eoriNumber))

          val result = performAction(FakeRequest())

          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson[AuthenticatedUser.Organisation](
            AuthenticatedUser
              .Organisation(
                None,
                Eori("GB000000000000002"),
                Some("John Smith")
              )
          )
        }

        "redirect to the start page when user NOT on the allow list" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Organisation) and eoriEnrolment(
              "GB000000000000003"
            ) and Some(
              ggCredentials
            ))

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result = performAction(FakeRequest())

          status(result)           shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some("/claim-back-import-duty-vat/unauthorised")
        }
      }

      "handling a logged in user with no eori enrolment" must {

        "redirect to unauthorised page" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Individual) and emptyEnrolments and Some(
              ggCredentials
            ))

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          }

          val result = performAction(FakeRequest())

          val redirectTo = redirectLocation(result)
          redirectTo shouldBe Some(
            routes.UnauthorisedController.unauthorised().url
          )
        }
      }

      "handling cases with incorrect type of credentials" must {

        "redirect to unauthorised page" in {
          val retrievalsResult =
            Future successful (Some(AffinityGroup.Individual) and someOtherEnrolment and Some(
              ggCredentials
            ))

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          }

          val result = performAction(FakeRequest())

          val redirectTo = redirectLocation(result)
          redirectTo shouldBe Some(
            routes.UnauthorisedController.unauthorised().url
          )
        }
      }

      "handling an unauthenticated user" must {

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

      "handling the case when an authorisation exception is thrown" must {

        "throw an exception" in {
          List[AuthorisationException](
            InsufficientEnrolments(),
            UnsupportedAffinityGroup(),
            UnsupportedAuthProvider(),
            UnsupportedCredentialRole(),
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

}
