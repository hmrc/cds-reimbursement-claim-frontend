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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.AnyContent
import play.api.mvc.MessagesRequest
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.routes as commonRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedRequestWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

import scala.concurrent.Future

class ClaimDeletedControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithJourneyGenerator[SecuritiesJourney] {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ClaimDeletedController = instanceOf[ClaimDeletedController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val eori = Eori("AB12345678901234Z")

  lazy val authenticatedRequest =
    AuthenticatedRequestWithRetrievedData(
      AuthenticatedUser.GovernmentGatewayAuthenticatedUser(
        None,
        eori,
        Some("John Smith")
      ),
      Some(UserType.Individual),
      messagesRequest
    )

  lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

  "ClaimDeletedController" when {

    def getContentsOfParagraph(p: Int)(implicit doc: Document): String =
      doc
        .select(s"#main-content > div > div > p:eq($p)")
        .html()
        .split("<a href")
        .head

    "show page" must {
      def showClaimDeletedPage(rh: Request[AnyContent] = FakeRequest()): Future[Result] = controller.show()(rh)

      "display the page if securities feature is enabled" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          showClaimDeletedPage(authenticatedRequest),
          messageFromMessageKey("claim-deleted.title"),
          implicit doc => messageFromMessageKey("claim-deleted.p1") should include(getContentsOfParagraph(1))
        )
      }
    }

    "start new claim" must {
      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] = controller.startNewClaim()(rh)

      "redirect to start of journey" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
          mockStoreSession(
            SessionData.empty
          )(Right(()))
        }

        val result = performAction(authenticatedRequest)
        checkIsRedirect(result, commonRoutes.ChooseClaimTypeController.show)
      }
    }

    "redirect to dashboard" must {
      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] = controller.redirectToDashboard()(rh)

      "redirect to the claims dashboard" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
          mockStoreSession(
            SessionData.empty
          )(Right(()))
        }

        val result = performAction(authenticatedRequest)
        checkIsRedirect(result, viewConfig.viewUploadUrl)
      }
    }
  }
}
