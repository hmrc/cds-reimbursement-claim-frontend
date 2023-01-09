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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.Future

class UploadMrnListControllerSpec extends PropertyBasedControllerSpec with AuthSupport with SessionSupport {

  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  val expectedUploadDocumentsLocation: String = "http://foo:3221/bar/upload-mrn-list"

  def mockInitializeCall(existingFile: Option[UploadedFile] = None) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFile.map(_.upscanReference).toList
      })
      .returning(Future.successful(Some(expectedUploadDocumentsLocation)))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: UploadMrnListController = instanceOf[UploadMrnListController]

  private def sessionWithScheduledDocumentAnswer(
    scheduledDocumentAnswer: Option[ScheduledDocumentAnswer]
  ): (SessionData, Nonce) = {
    val nonce               = Nonce.random
    val draftC285Claim      =
      DraftClaim.blank.copy(
        nonce = nonce,
        scheduledDocumentAnswer = scheduledDocumentAnswer
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

  "UploadMrnListController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "redirect to the 'Upload Mrn List' when no file uploaded yet" in {
        val (session, _) = sessionWithScheduledDocumentAnswer(None)
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

      "redirect to the 'Upload Mrn List' when some file uploaded already" in {
        val uploadDocument = buildUploadDocument(UUID.randomUUID().toString())
        val (session, _)   = sessionWithScheduledDocumentAnswer(Some(ScheduledDocumentAnswer(uploadDocument)))
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

    "'Upload Mrn List' submitted the callback" must {

      def performAction(callback: UploadMrnListCallback): Future[Result] =
        controller.callback()(FakeRequest().withJsonBody(Json.toJson(callback)))

      val uploadDocument = buildUploadDocument(UUID.randomUUID().toString())

      val callbackPayload: UploadMrnListCallback =
        UploadMrnListCallback(
          nonce = Nonce.random,
          uploadedFiles = Seq(uploadDocument)
        )

      "return 204 if callback accepted" in {
        val (session, nonce) = sessionWithScheduledDocumentAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.withUpdatedC285Claim(
              _.copy(scheduledDocumentAnswer = Some(ScheduledDocumentAnswer(uploadDocument)))
            )
          )(Right(()))
        }
        val result = performAction(callbackPayload.copy(nonce = nonce))
        status(result) shouldBe 204
      }

      "return 400 if callback rejected because of invalid nonce" in {
        val (session, _) = sessionWithScheduledDocumentAnswer(None)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result       = performAction(callbackPayload.copy(nonce = Nonce.random))
        status(result) shouldBe 400
      }

      "return 400 if callback rejected because of invalid request" in {
        val (session, _) = sessionWithScheduledDocumentAnswer(None)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result       = controller.callback()(FakeRequest().withJsonBody(Json.parse("""{"foo":"bar"}""")))
        status(result) shouldBe 400
      }

    }

    "'Upload Mrn List' continue action" must {

      def performAction(): Future[Result] =
        controller.continue()(FakeRequest())

      "redirect to the next page" in {
        val (session, _) = sessionWithScheduledDocumentAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        checkIsRedirect(
          performAction(),
          OverpaymentsRoutes.CheckContactDetailsController.show(JourneyBindable.Scheduled)
        )
      }
    }

  }

  def buildUploadDocument(id: String) = UploadedFile(
    upscanReference = s"upscan-reference-$id",
    fileName = s"file-name-$id",
    downloadUrl = s"download-url-$id",
    uploadTimestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0L), ZoneId.of("Europe/London")),
    checksum = "A" * 64,
    fileMimeType = s"application/$id",
    fileSize = Some(12345)
  )

}
