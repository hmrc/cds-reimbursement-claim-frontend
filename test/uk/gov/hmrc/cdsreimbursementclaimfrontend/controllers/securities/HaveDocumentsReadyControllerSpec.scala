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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.jdk.CollectionConverters.*
import scala.concurrent.Future

class HaveDocumentsReadyControllerSpec
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

  val controller: HaveDocumentsReadyController = instanceOf[HaveDocumentsReadyController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Securities)

  "HaveDocumentsReadyController" when {

    def getContentsOfParagraph(p: Int)(implicit doc: Document): String =
      doc
        .select(s"#main-content > div > div > p:eq($p)")
        .html()
        .split("<a href")
        .head

    "show page" must {
      def showHaveDocumentsReadyPage: Future[Result] = controller.show(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(showHaveDocumentsReadyPage) shouldBe NOT_FOUND
      }

      "display the page if securities feature is enabled" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        val expectedContinueUrl =
          if journey.getReasonForSecurity.exists(ntas.contains) then routes.ChooseExportMethodController.show.url
          else routes.ConfirmFullRepaymentController.showFirst.url

        checkPageIsDisplayed(
          showHaveDocumentsReadyPage,
          messageFromMessageKey(s"have-documents-ready.title"),
          implicit doc =>
            if journey.getReasonForSecurity.contains(MissingPreferenceCertificate) then
              messageFromMessageKey("have-documents-ready.securities.mdp.p1") should include(getContentsOfParagraph(1))
            else messageFromMessageKey("have-documents-ready.p1")             should include(getContentsOfParagraph(1))
            doc
              .select("a.govuk-button")
              .asScala
              .find(_.text() == "Continue")
              .map(_.attr("href"))                                          shouldBe Some(expectedContinueUrl)
        )
      }

      "display the page with correct continue url when declaration has only one security deposit" in {
        val journey        = buildCompleteJourneyGen(numberOfSecurityDetails = Some(1)).sample.get
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          showHaveDocumentsReadyPage,
          messageFromMessageKey(s"have-documents-ready.title"),
          implicit doc =>
            messageFromMessageKey("have-documents-ready.p1") should include(getContentsOfParagraph(1))
            doc
              .select("a.govuk-button")
              .asScala
              .find(_.text() == "Continue")
              .map(_.attr("href"))                         shouldBe Some(routes.ConfirmSingleDepositRepaymentController.show.url)
        )
      }

      "display the page with correct continue url when declaration has multiple security deposits" in {
        val journey        = buildCompleteJourneyGen(numberOfSecurityDetails = Some(3)).sample.get
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        val expectedContinueUrl =
          if journey.getReasonForSecurity.exists(ntas.contains) then routes.ChooseExportMethodController.show.url
          else routes.ConfirmFullRepaymentController.showFirst.url

        checkPageIsDisplayed(
          showHaveDocumentsReadyPage,
          messageFromMessageKey(s"have-documents-ready.title"),
          implicit doc =>
            messageFromMessageKey("have-documents-ready.p1") should include(getContentsOfParagraph(1))
            doc
              .select("a.govuk-button")
              .asScala
              .find(_.text() == "Continue")
              .map(_.attr("href"))                         shouldBe Some(expectedContinueUrl)
        )
      }
    }
  }
}
