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
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.concurrent.Future

class CheckDeclarationDetailsWithoutSecuritiesSelectionControllerSpec
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

  val controller: CheckDeclarationDetailsWithoutSecuritiesSelectionController =
    instanceOf[CheckDeclarationDetailsWithoutSecuritiesSelectionController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val messagesKey: String = "check-import-declaration-details"

  def validatePage(doc: Document) = {}

  "CheckDeclarationDetailsWithoutSecuritiesSelectionController" when {
    "show page" must {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyIPRGen,
          journeyBuilder = buildSecuritiesJourneyReadyForIPR
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messages("check-import-declaration-details.title"),
          doc => validatePage(doc)
        )
      }

      "submit" must {

        def performAction(data: (String, String)*): Future[Result] =
          controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

        "redirect to CheckTotalImportDischarged" in forSomeWith(
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
            performAction(),
            routes.CheckTotalImportDischargedController.show
          )
        }
      }
    }
  }

}
