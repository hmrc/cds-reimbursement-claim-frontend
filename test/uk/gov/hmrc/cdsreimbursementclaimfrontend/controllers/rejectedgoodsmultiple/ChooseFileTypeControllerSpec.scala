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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

class ChooseFileTypeControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseFileTypeController = instanceOf[ChooseFileTypeController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "choose-file-type"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def validateChooseFileTypePage(doc: Document, journey: RejectedGoodsMultipleJourney) = {
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Additional supporting documents", "AdditionalSupportingDocuments"),
      ("Calculation worksheet", "CalculationWorksheet"),
      ("Commercial invoice", "CommercialInvoice"),
      ("Correspondence between trader and agent", "CorrespondenceTrader"),
      ("Documentary proof that the goods are faulty or not what you ordered", "DocumentaryProofFaultyOrNotWhatOrdered"),
      ("Import and export declaration", "ImportAndExportDeclaration"),
      ("Letter of authority", "LetterOfAuthority"),
      ("Proof of export or destruction", "ProofOfExportOrDestruction"),
      (
        if (journey.answers.supportingEvidences.isEmpty)
          "I have no documents to upload"
        else
          "I have no more documents to upload",
        "none"
      )
    )
    hasContinueButton(doc)
  }

  "ChooseFileTypeController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, journeyWithMrnAndDD)
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

      "not succeed if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction(("choose-file-type", "AdditionalSupportingDocuments"))) shouldBe NOT_FOUND
      }

      "redirect to choose files when valid document type selection" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
          mockStoreSession(
            SessionData(
              journeyWithMrnAndDD.submitDocumentTypeSelection(UploadDocumentType.AdditionalSupportingDocuments)
            )
          )(Right(()))
        }
        checkIsRedirect(
          performAction("choose-file-type" -> "AdditionalSupportingDocuments"),
          routes.UploadFilesController.show()
        )
      }

      "re-display the page when invalid document type selection" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
        }
        checkPageIsDisplayed(
          performAction("choose-file-type" -> "Foo"),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, journeyWithMrnAndDD),
          expectedStatus = 400
        )
      }

      "re-display the page when nothing has been selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, journeyWithMrnAndDD),
          expectedStatus = 400
        )
      }

      "redirect to CYA when selected 'no documents to upload'" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
        }
        checkIsRedirect(
          performAction("choose-file-type" -> "none"),
          routes.CheckYourAnswersController.show()
        )
      }

    }

  }

}
