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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.AnyContent
import play.api.mvc.MessagesRequest
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.contentAsString
import play.api.test.Helpers.status
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedRequestWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

import scala.concurrent.Future

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]
  lazy val controller: StartController       = instanceOf[StartController]

  "Start controller" when {

    "handling requests to start a journey" when {

      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] = controller.start()(rh)

      "there is a signed in non gg user user and there is no current journey status" must {

        "redirect the user to the `we only support gg page`" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              AuthenticatedUser.NonGovernmentGatewayAuthenticatedUser("some auth provider"),
              Some(UserType.NonGovernmentGatewayUser),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData = SessionData.empty

          inSequence {
            mockAuthWithNonGGUserRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction(authenticatedRequest)
          checkIsRedirect(result, routes.StartController.weOnlySupportGG())
        }
      }

      "there is a signed in individual gg user user" must {

        "redirect the individual to the check EORI details page" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              AuthenticatedUser.Individual(
                None,
                Eori("AB12345678901234Z"),
                Some(Name(Some("John Smith"), Some("Smith")))
              ),
              Some(UserType.Individual),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData = SessionData.empty

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction(authenticatedRequest)
          checkIsRedirect(result, commonRoutes.CheckEoriDetailsController.show())

        }
      }

      "there is a signed in organisation user" must {

        "redirect the organisation to the check EORI details page" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              AuthenticatedUser.Organisation(
                Some(Email("email")),
                Eori("AB12345678901234Z"),
                Some(Name(Some("John Smith"), Some("Smith")))
              ),
              Some(UserType.Organisation),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          inSequence {
            mockAuthWithOrgWithEoriEnrolmentRetrievals()
            mockGetSession(SessionData.empty)
          }

          val result = performAction(authenticatedRequest)
          checkIsRedirect(result, commonRoutes.CheckEoriDetailsController.show())
        }
      }

      "there is non government way journey status" must {

        "redirect the user to the `we only support gg page`" in {
          inSequence {
            mockAuthWithNonGGUserRetrievals()
            mockGetSession(SessionData.empty)
          }

          val result = performAction()
          checkIsRedirect(result, routes.StartController.weOnlySupportGG())
        }
      }
    }

    "handling requests to sign out and sign in" must {

      def performAction(sessionData: Seq[(String, String)]): Future[Result] =
        controller.signOutAndSignIn()(
          FakeRequest().withSession(sessionData: _*)
        )

      "kill the session and redirect to service" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals()
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq("key" -> "value"))
        checkIsRedirect(result, routes.StartController.start())
        session(result).data shouldBe Map.empty
      }

    }

    "handling requests to display the 'we only support gg' page" must {

      def performAction(): Future[Result] =
        controller.weOnlySupportGG()(FakeRequest())

      "display the page" in {
        inSequence {
          mockAuthWithNonGGUserRetrievals()
          mockGetSession(SessionData.empty)
        }

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("we-only-support-gg.title")
        )
      }
    }

    "handling requests to sign out and register for GG" must {

      def performAction(sessionData: Seq[(String, String)]): Future[Result] =
        controller.signOutAndRegisterForGG()(
          FakeRequest().withSession(sessionData: _*)
        )

      "trash the session and redirect to the gg registration service" in {
        inSequence {
          mockAuthWithNonGGUserRetrievals()
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq("key" -> "value"))
        checkIsRedirect(result, viewConfig.ggCreateAccountUrl)
        session(result).data shouldBe Map.empty
      }

    }

    "handling requests to keep alive" must {

      "return an ok response with an empty body" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Right(None))
        }

        val result = controller.keepAlive()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe ""
      }

    }

    "handling requests to display the timed out page" must {

      "display the page" in {
        val result = controller.timedOut()(FakeRequest())
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("timed-out.title")
        )
      }
    }
  }

}
