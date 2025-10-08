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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType

import scala.concurrent.Future

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

  private val messagesKey: String = "choose-file-type"

  def validateChooseFileTypePage(doc: Document, claim: RejectedGoodsMultipleClaim) = {
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Additional supporting documents", "AdditionalSupportingDocuments"),
      ("Calculation worksheet", "CalculationWorksheet"),
      ("Commercial invoice", "CommercialInvoice"),
      ("Correspondence between trader and agent", "CorrespondenceTrader"),
      ("Documentary proof that the goods are faulty or not what you ordered", "DocumentaryProofFaultyOrNotWhatOrdered"),
      ("Import or export declaration", "ImportAndExportDeclaration"),
      ("Proof of authority", "LetterOfAuthority"),
      ("Proof of export or destruction", "ProofOfExportOrDestruction")
    ) ++ (
      if claim.answers.supportingEvidences.nonEmpty then List(("I have no more documents to upload", "none"))
      else Nil
    )
    hasContinueButton(doc)
  }

  "ChooseFileTypeController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claimWithMrnAndDeclaration))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, claimWithMrnAndDeclaration)
        )

      }

      "display the page when in change mode" in {
        forAll(completeClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateChooseFileTypePage(doc, claim)
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
          mockGetSession(SessionData(claimWithMrnAndDeclaration))
          mockStoreSession(
            SessionData(
              claimWithMrnAndDeclaration.submitDocumentTypeSelection(UploadDocumentType.AdditionalSupportingDocuments)
            )
          )(Right(()))
        }
        checkIsRedirect(
          performAction("choose-file-type" -> "AdditionalSupportingDocuments"),
          routes.UploadFilesController.show
        )
      }

      "re-display the page when invalid document type selection" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claimWithMrnAndDeclaration))
        }
        checkPageIsDisplayed(
          performAction("choose-file-type" -> "Foo"),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, claimWithMrnAndDeclaration),
          expectedStatus = 400
        )
      }

      "re-display the page when nothing has been selected" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claimWithMrnAndDeclaration))
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseFileTypePage(doc, claimWithMrnAndDeclaration),
          expectedStatus = 400
        )
      }

      "redirect to CYA when selected 'no documents to upload'" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claimWithMrnAndDeclaration))
        }
        checkIsRedirect(
          performAction("choose-file-type" -> "none"),
          routes.CheckYourAnswersController.show
        )
      }

    }

  }

}
