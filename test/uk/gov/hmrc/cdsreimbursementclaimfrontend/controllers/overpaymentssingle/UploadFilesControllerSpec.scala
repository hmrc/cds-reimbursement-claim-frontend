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

import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Json
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswerList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.Future

class UploadFilesControllerSpec extends PropertyBasedControllerSpec with AuthSupport with SessionSupport {

  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  val expectedUploadDocumentsLocation: String = "http://foo:7289/bar/choose-files"

  def mockInitializeCall(existingFile: Option[UploadedFile] = None) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where[UploadDocumentsConnector.Request, HeaderCarrier] { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFile.map(_.upscanReference).toList
      })
      .returning(Future.successful(Some(expectedUploadDocumentsLocation)))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  val controller: UploadFilesController = instanceOf[UploadFilesController]

  private def sessionWithSupportingEvidenceAnswer(
    documentTypeAnswer: Option[UploadDocumentType],
    supportingEvidencesAnswer: Option[SupportingEvidencesAnswerList]
  ): (SessionData, Nonce) = {
    val nonce               = Nonce.random
    val draftC285Claim      =
      DraftClaim.blank.copy(
        nonce = nonce,
        documentTypeAnswer = documentTypeAnswer,
        supportingEvidencesAnswer = supportingEvidencesAnswer
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = JourneyStatus.FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)

    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      nonce
    )

  }

  val uploadDocument: UploadedFile = buildUploadDocument(UUID.randomUUID().toString)

  "UploadFilesController in single" when {

    "Show choose files page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "redirect to the 'Choose File Type' page when no document type selected and none file uploaded yet" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = None,
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.ChooseFileTypeController.show
        )
      }

      "redirect to the 'Choose File Type' page when no document type selected and some uploads exists" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = None,
          supportingEvidencesAnswer = Some(List(uploadDocument))
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.ChooseFileTypeController.show
        )
      }

      "redirect to the 'Choose Files' page when no file uploaded yet" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.AirWayBill),
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockInitializeCall()
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to the 'Choose Files' when some file uploaded already" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.CommercialInvoice),
          supportingEvidencesAnswer = Some(List(uploadDocument))
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockInitializeCall(Some(uploadDocument))
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

    }

    "Redirect to the file upload summary page" must {

      def performAction(): Future[Result] = controller.summary(FakeRequest())

      "redirect to the 'Choose File Type' page when no document type selected and none file uploaded yet" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = None,
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.ChooseFileTypeController.show
        )
      }

      "redirect to the 'Choose File Type' page when no document type selected and some uploads exists" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = None,
          supportingEvidencesAnswer = Some(List(uploadDocument))
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.ChooseFileTypeController.show
        )
      }

      "redirect to the 'Uploaded Files Summary' page when no file uploaded yet" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.AirWayBill),
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockInitializeCall()
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to the 'Uploded Files Summary' when some files has been uploaded already" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.CommercialInvoice),
          supportingEvidencesAnswer = Some(List(uploadDocument))
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockInitializeCall(Some(uploadDocument))
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to the internal summary page when no file uploaded yet" in {
        featureSwitch.enable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.AirWayBill),
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockInitializeCall()
        }

        checkIsRedirect(
          performAction(),
          uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.routes.UploadDocumentsController.summary()
        )
      }

      "redirect to the internal summary when some files has been uploaded already" in {
        featureSwitch.enable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.CommercialInvoice),
          supportingEvidencesAnswer = Some(List(uploadDocument))
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockInitializeCall(Some(uploadDocument))
        }

        checkIsRedirect(
          performAction(),
          uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.routes.UploadDocumentsController.summary()
        )
      }

    }

    "'Choose Files' page submitted the callback" must {

      def performAction(callback: UploadDocumentsCallback): Future[Result] =
        controller.callback(FakeRequest().withJsonBody(Json.toJson(callback)))

      val callbackPayload: UploadDocumentsCallback =
        UploadDocumentsCallback(
          nonce = Nonce.random,
          uploadedFiles = Seq(uploadDocument),
          cargo = UploadDocumentType.CorrespondenceTrader
        )

      "return 204 if callback accepted when no files uploaded yet" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, nonce) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.CorrespondenceTrader),
          supportingEvidencesAnswer = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.withUpdatedC285Claim(
              _.copy(supportingEvidencesAnswer =
                Some(List(uploadDocument.copy(cargo = Some(UploadDocumentType.CorrespondenceTrader))))
              )
            )
          )(Right(()))
        }
        val result = performAction(callbackPayload.copy(nonce = nonce))
        status(result) shouldBe 204
      }

      "return 204 if callback accepted and replace existing files" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, nonce) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.CorrespondenceTrader),
          supportingEvidencesAnswer = Some(List(buildUploadDocument("1"), buildUploadDocument("2")))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.withUpdatedC285Claim(
              _.copy(supportingEvidencesAnswer =
                Some(List(uploadDocument.copy(cargo = Some(UploadDocumentType.CorrespondenceTrader))))
              )
            )
          )(Right(()))
        }
        val result = performAction(callbackPayload.copy(nonce = nonce))
        status(result) shouldBe 204
      }

      "return 400 if callback rejected because of invalid nonce" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.BillOfLading),
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result       = performAction(callbackPayload.copy(nonce = Nonce.random))
        status(result) shouldBe 400
      }

      "return 400 if callback rejected because of invalid request" in {
        featureSwitch.disable(Feature.InternalUploadDocuments)

        val (session, _) = sessionWithSupportingEvidenceAnswer(
          documentTypeAnswer = Some(UploadDocumentType.AirWayBill),
          supportingEvidencesAnswer = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result       = controller.callback(FakeRequest().withJsonBody(Json.parse("""{"foo":"bar"}""")))
        status(result) shouldBe 400
      }

    }

  }

  def buildUploadDocument(id: String): UploadedFile = UploadedFile(
    upscanReference = s"upscan-reference-$id",
    fileName = s"file-name-$id",
    downloadUrl = s"download-url-$id",
    uploadTimestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0L), ZoneId.of("Europe/London")),
    checksum = "A" * 64,
    fileMimeType = s"application/$id",
    fileSize = Some(12345)
  )

}
