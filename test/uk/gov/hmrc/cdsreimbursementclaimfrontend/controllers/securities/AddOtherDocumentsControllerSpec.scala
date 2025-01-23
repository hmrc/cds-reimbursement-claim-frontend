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

import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*

import scala.concurrent.Future

class AddOtherDocumentsControllerSpec
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

  val controller: AddOtherDocumentsController = instanceOf[AddOtherDocumentsController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  "AddOtherDocumentsController" when {

    "show page" must {

      def performAction: Future[Result] = controller.show(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction) shouldBe NOT_FOUND
      }

      "display the page if securities feature is enabled" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyIPRGen,
          journeyBuilder = buildSecuritiesJourneyReadyForIPR
        )
      ) { case (journey, _) =>
        val updatedSession = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey(s"add-other-documents.title")
        )
      }

    }

    "submitting Yes/No form" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "select 'Yes' should redirect to choose document type page" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyIPRGen,
          journeyBuilder = buildSecuritiesJourneyReadyForIPR
        )
      ) { case (journey, _) =>
        val updatedSession = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          performAction("add-other-documents" -> "true"),
          routes.ChooseFileTypeController.show
        )
      }

      "select 'No' should redirect to choose payee type page" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyIPRGen,
          journeyBuilder = buildSecuritiesJourneyReadyForIPR
        )
      ) { case (journey, _) =>
        val updatedSession = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          performAction("add-other-documents" -> "false"),
          routes.ChoosePayeeTypeController.show
        )
      }

    }

  }

}
