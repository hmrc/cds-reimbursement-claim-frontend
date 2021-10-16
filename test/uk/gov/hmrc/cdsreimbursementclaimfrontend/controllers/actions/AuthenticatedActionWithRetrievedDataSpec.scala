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

import julienrf.json.derived
import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Name, Retrieval, ~}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{ControllerSpec, RetrievalOps, SessionSupport, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionWithRetrievedDataSpec
    extends ControllerSpec
    with MockFactory
    with SessionSupport
    with AuthActionSpec {

  val authenticatedAction =
    new AuthenticatedActionWithRetrievedData(
      mockAuthConnector,
      config,
      instanceOf[ErrorHandler],
      mockSessionCache
    )

  implicit val format: OFormat[RetrievedUserType] = derived.oformat[RetrievedUserType]()

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

  val retrievals: Retrieval[Option[AffinityGroup] ~ Option[String] ~ Enrolments ~ Option[Credentials] ~ Option[Name]] =
    Retrievals.affinityGroup and
      Retrievals.email and
      Retrievals.allEnrolments and
      Retrievals.credentials and
      Retrievals.name

  val emptyEnrolments: Enrolments = Enrolments(Set.empty)

  val eori: Eori = sample[Eori]

  val eoriEnrolment: Enrolments = Enrolments(
    Set(
      Enrolment(
        "HMRC-CUS-ORG",
        Seq(
          EnrolmentIdentifier(
            "EORINumber",
            eori.value
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

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  val (ggCredentials, ggCredId) = Credentials("id", "GovernmentGateway") -> GGCredId("id")

  "Authenticated action with retrieved data" when {

    "handling a user who has logged in with an auth provider which isn't gg" must {

      "return the auth provider id" in {
        val providerType     = "other provider"
        val retrievalsResult =
          Future successful (new ~(Some(AffinityGroup.Organisation), Some("email")) and emptyEnrolments and Some(
            Credentials("id", providerType)
          ) and Some(Name(Some("John Smith"), Some("Smith"))))

        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())

        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson[RetrievedUserType](
          RetrievedUserType.NonGovernmentGatewayRetrievedUser(providerType)
        )
      }

    }

    "handling a logged in user with an eori enrolment" must {

      "return the signed in details for an individual" in {
        val retrievalsResult =
          Future successful (new ~(Some(AffinityGroup.Individual), Some("email")) and eoriEnrolment and Some(
            ggCredentials
          ) and Some(Name(Some("John Smith"), Some("Smith"))))

        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())

        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson[RetrievedUserType](
          RetrievedUserType
            .Individual(
              GGCredId("id"),
              Some(Email("email")),
              eori,
              Some(models.Name(Some("John Smith"), Some("Smith")))
            )
        )
      }

      "return the signed in details for an organisation" in {
        val retrievalsResult =
          Future successful (new ~(Some(AffinityGroup.Organisation), Some("email")) and eoriEnrolment and Some(
            ggCredentials
          ) and None)

        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())

        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson[RetrievedUserType](
          RetrievedUserType
            .Organisation(
              GGCredId("id"),
              eori,
              None
            )
        )
      }
    }

    "handling a logged in user with no eori enrolment" must {

      "redirect to unauthorised page" in {
        val retrievalsResult =
          Future successful (new ~(Some(AffinityGroup.Individual), Some("email")) and emptyEnrolments and Some(
            ggCredentials
          ) and Some(Name(Some("John Smith"), Some("Smith"))))

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
          Future successful (new ~(Some(AffinityGroup.Individual), Some("email")) and someOtherEnrolment and Some(
            ggCredentials
          ) and Some(Name(Some("John Smith"), Some("Smith"))))

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
              s"$signInUrl?continue_url=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin"
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
