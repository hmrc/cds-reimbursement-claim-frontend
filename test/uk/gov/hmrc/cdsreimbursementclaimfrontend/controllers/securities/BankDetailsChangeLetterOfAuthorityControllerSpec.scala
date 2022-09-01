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
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, PropertyBasedControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Feature, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.{SummaryMatchers, TestWithJourneyGenerator}

import scala.concurrent.Future

class BankDetailsChangeLetterOfAuthorityControllerSpec
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

  val controller: BankDetailsChangeLetterOfAuthorityController = instanceOf[BankDetailsChangeLetterOfAuthorityController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val confirmBodMessagesKey: String = "bank_account_letter_of_authority"

  private def letterOfAuthorityPage: Future[Result] = controller.show()(FakeRequest())

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  "BankDetailsChangeLetterOfAuthorityController" when {

    def getContentsOfParagraph(p: Int)(implicit doc: Document): String =
      doc.select(s"#main-content > div > div > p:eq($p)").html()

    "show page" must {

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(letterOfAuthorityPage) shouldBe NOT_FOUND
      }

      "display the page if securities feature is enabled" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          letterOfAuthorityPage,
          messageFromMessageKey(s"$confirmBodMessagesKey.title"),
          implicit doc => {
            messageFromMessageKey(s"$confirmBodMessagesKey.p1") shouldBe getContentsOfParagraph(1)
            messageFromMessageKey(s"$confirmBodMessagesKey.p2") shouldBe getContentsOfParagraph(2)
          }
        )
      }

    }

    "submitting Yes/No form" must {

      def submitLetterOfAuthorityAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "select 'Yes' should redirect to choose bank account type page" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          submitLetterOfAuthorityAction(confirmBodMessagesKey -> "true"),
          routes.ChooseBankAccountTypeController.show()
        )
      }

      "select 'No' should redirect to check bank details controller page" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          submitLetterOfAuthorityAction(confirmBodMessagesKey -> "false"),
          routes.CheckBankDetailsController.show()
        )
      }
    }

  }

}
