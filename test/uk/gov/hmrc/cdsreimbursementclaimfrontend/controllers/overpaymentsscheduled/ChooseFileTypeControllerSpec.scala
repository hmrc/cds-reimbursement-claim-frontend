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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers

import scala.concurrent.Future

class ChooseFileTypeControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseFileTypeController = instanceOf[ChooseFileTypeController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "supporting-evidence.choose-document-type"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  @annotation.nowarn
  def validateChooseFileTypePage(doc: Document, journey: OverpaymentsScheduledJourney) = {
    radioItems(doc) should containOnlyPairsOf(
      Seq(
        ("Air waybill", "AirWayBill"),
        ("Bill of lading", "BillOfLading"),
        ("Commercial invoice", "CommercialInvoice"),
        ("Correspondence between trader and agent", "CorrespondenceTrader"),
        ("Import and export declaration", "ImportAndExportDeclaration"),
        ("Packing list", "PackingList"),
        ("Letter of authority", "ProofOfAuthority"),
        ("Substitute entry", "SubstituteEntry"),
        ("Other documents", "Other")
      )
    )
    hasContinueButton(doc)
  }

  "ChooseFileTypeController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, journeyWithMrnAndDeclaration)
        )

      }

      "display the page when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateChooseFileTypePage(doc, journey)
          )
        }
      }
    }

    "submitted the document type" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not succeed if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction(("choose-file-type", "SubstituteEntry"))) shouldBe NOT_FOUND
      }

      "redirect to choose files when valid document type selection" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
          mockStoreSession(
            SessionData(
              journeyWithMrnAndDeclaration.submitDocumentTypeSelection(UploadDocumentType.SubstituteEntry)
            )
          )(Right(()))
        }
        checkIsRedirect(
          performAction("choose-file-type" -> "SubstituteEntry"),
          routes.UploadFilesController.show
        )
      }

      "re-display the page when invalid document type selection" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
        }
        checkPageIsDisplayed(
          performAction("choose-file-type" -> "Foo"),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, journeyWithMrnAndDeclaration),
          expectedStatus = 400
        )
      }

      "re-display the page when nothing has been selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, journeyWithMrnAndDeclaration),
          expectedStatus = 400
        )
      }

    }

  }

}
