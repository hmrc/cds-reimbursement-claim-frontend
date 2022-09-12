/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.http.Status.NOT_FOUND
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.concurrent.Future

class BillOfDischarge4ControllerSpec
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

  val controller: BillOfDischarge4Controller = instanceOf[BillOfDischarge4Controller]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val confirmBodMessagesKey: String = "bill-of-discharge"

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  "BillOfDischargeController" when {

    def getContentsOfParagraph(p: Int)(implicit doc: Document): String =
      doc
        .select(s"#main-content > div > div > p:eq($p)")
        .html()
        .split("<a href")
        .head

    "show page" must {
      def showBod4Page: Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(showBod4Page) shouldBe NOT_FOUND
      }

      "display the page if securities feature is enabled (BOD4)" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          showBod4Page,
          messageFromMessageKey(s"$confirmBodMessagesKey.bod4.title"),
          implicit doc => {
            messageFromMessageKey(s"$confirmBodMessagesKey.bod4.p1") should include(getContentsOfParagraph(1))
            messageFromMessageKey(s"$confirmBodMessagesKey.p2")      should include(getContentsOfParagraph(2))
          }
        )
      }
    }

    "submitting Yes/No form" must {

      def submitBod4Action(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "select 'Yes' should redirect to select securities page (BOD4)" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          submitBod4Action(confirmBodMessagesKey -> "true"),
          routes.SelectSecuritiesController.showFirst()
        )
      }

      "select 'No' should redirect to 'Bill of Discharge' error page (BOD4)" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          submitBod4Action(confirmBodMessagesKey -> "false"),
          routes.BillOfDischarge4Controller.invalid()
        )
      }
    }

    "Bill of Discharge Error page" must {
      def invalidBod4Action: Future[Result] = controller.invalid()(FakeRequest())

      val errorBodMessagesKey: String = s"$confirmBodMessagesKey-error"

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(invalidBod4Action) shouldBe NOT_FOUND
      }

      "display the page if securities feature is enabled (BOD4)" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          invalidBod4Action,
          messageFromMessageKey(s"$errorBodMessagesKey.title"),
          implicit doc => {
            messageFromMessageKey(s"$errorBodMessagesKey.p1")      should include(getContentsOfParagraph(1))
            messageFromMessageKey(s"$errorBodMessagesKey.bod4.p2") should include(getContentsOfParagraph(2))
            messageFromMessageKey(s"$errorBodMessagesKey.bod4.p3") should include(getContentsOfParagraph(3))
            messageFromMessageKey(s"$errorBodMessagesKey.p4")      should include(getContentsOfParagraph(4))
            messageFromMessageKey(s"$errorBodMessagesKey.p5")      should include(getContentsOfParagraph(5))
          }
        )
      }

    }

  }

}
