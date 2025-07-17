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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class HaveDocumentsReadyControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithJourneyGenerator[OverpaymentsMultipleJourney] {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: HaveDocumentsReadyController = instanceOf[HaveDocumentsReadyController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(OverpaymentsMultipleJourney.empty(exampleEori))

  "HaveDocumentsReadyController" when {

    def getContentsOfParagraph(p: Int)(implicit doc: Document): String =
      doc
        .select(s"#main-content > div > div > p:eq($p)")
        .html()
        .split("<a href")
        .head

    "show page" must {
      def showHaveDocumentsReadyPage: Future[Result] = controller.show(FakeRequest())

      "display the page" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          showHaveDocumentsReadyPage,
          messageFromMessageKey(s"have-documents-ready.title"),
          implicit doc =>
            messageFromMessageKey("have-documents-ready.overpayments.p1") should include(getContentsOfParagraph(1))
            doc
              .select("a.govuk-button")
              .asScala
              .find(_.text() == "Continue")
              .map(_.attr("href"))                                      shouldBe Some(routes.EnterMovementReferenceNumberController.showFirst.url)
        )
      }
    }
  }
}
