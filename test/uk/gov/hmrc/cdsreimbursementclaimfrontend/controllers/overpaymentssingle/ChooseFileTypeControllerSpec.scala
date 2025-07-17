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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
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

  private val messagesKey: String = "choose-file-type"

  def validateChooseFileTypePage(doc: Document, journey: OverpaymentsSingleJourney) = {
    radioItems(doc) should containOnlyPairsOf(
      Seq(
        ("Air waybill", "AirWayBill"),
        ("Bill of lading", "BillOfLading"),
        ("Commercial invoice", "CommercialInvoice"),
        ("Correspondence between trader and agent", "CorrespondenceTrader"),
        ("Import or export declaration", "ImportAndExportDeclaration"),
        ("Packing list", "PackingList"),
        ("Proof of authority", "ProofOfAuthority"),
        ("Substitute entry", "SubstituteEntry"),
        ("Other", "Other")
      )
    )
    hasContinueButton(doc)
  }

  "ChooseFileTypeController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
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
            mockAuthWithDefaultRetrievals()
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
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "redirect to choose files when valid document type selection" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
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
          mockAuthWithDefaultRetrievals()
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
          mockAuthWithDefaultRetrievals()
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
