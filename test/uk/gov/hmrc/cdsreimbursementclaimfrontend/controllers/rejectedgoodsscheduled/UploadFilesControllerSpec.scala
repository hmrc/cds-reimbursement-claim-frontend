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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.scalatest.BeforeAndAfterEach
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import play.api.libs.json.Json

class UploadFilesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  val expectedUploadDocumentsLocation: String = "/upload-files"

  def mockInitializeCall(existingFiles: Seq[UploadedFile] = Seq.empty) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFiles.map(_.upscanReference)
      })
      .returning(Future.successful(Some(expectedUploadDocumentsLocation)))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: UploadFilesController = instanceOf[UploadFilesController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  "UploadFilesController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to 'Upload Documents' when document type set and no files uploaded yet" in {
        val journey =
          RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .submitDocumentTypeSelection(UploadDocumentType.AirWayBill)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData(
              journey.submitDocumentTypeSelection(UploadDocumentType.CommercialInvoice)
            )
          )
          mockInitializeCall()
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"http://localhost:10100$expectedUploadDocumentsLocation")
        )
      }

      "redirect to 'Upload Documents' when document type set and some files uploaded already" in {
        val journey =
          RejectedGoodsScheduledJourney
            .empty(exampleEori)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData(
              journey
                .submitDocumentTypeSelection(UploadDocumentType.AirWayBill)
                .receiveUploadedFiles(UploadDocumentType.AirWayBill, journey.answers.nonce, Seq(uploadDocument))
                .getOrFail
            )
          )
          mockInitializeCall(Seq(uploadDocument.copy(cargo = Some(UploadDocumentType.AirWayBill))))
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"http://localhost:10100$expectedUploadDocumentsLocation")
        )
      }

      "redirect to 'Upload Documents' if journey has complete answers and document type set" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData(
                journey.submitDocumentTypeSelection(UploadDocumentType.CommercialInvoice)
              )
            )
            mockInitializeCall(journey.answers.supportingEvidences)
          }

          checkIsRedirect(
            performAction(),
            Call("GET", s"http://localhost:10100$expectedUploadDocumentsLocation")
          )
        }
      }

      "redirect to document type selection if journey has complete answers but document type not set" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.ChooseFileTypeController.show()
          )
        }
      }

    }

    "'Upload Documents' submitted the callback" must {

      def performAction(callback: UploadDocumentsCallback): Future[Result] =
        controller.submit()(FakeRequest().withJsonBody(Json.toJson(callback)))

      val callbackPayload: UploadDocumentsCallback =
        UploadDocumentsCallback(
          nonce = Nonce.random,
          uploadedFiles = Seq(uploadDocument),
          cargo = UploadDocumentType.CommercialInvoice
        )

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction(callbackPayload)) shouldBe NOT_FOUND
      }

      "return 204 if callback accepted" in {
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(
              journey
                .receiveUploadedFiles(UploadDocumentType.CommercialInvoice, journey.answers.nonce, Seq(uploadDocument))
                .getOrFail
            )
          )(Right(()))
        }
        val result  = performAction(callbackPayload.copy(nonce = journey.answers.nonce))
        status(result) shouldBe 204
      }

      "return 400 if callback rejected because of invalid nonce" in {
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData(journey)
          )
        }
        val result  = performAction(callbackPayload.copy(nonce = Nonce.random))
        status(result) shouldBe 400
      }

      "return 400 if callback rejected because of invalid request" in {
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData(journey)
          )
        }
        val result  = controller.submit()(FakeRequest().withJsonBody(Json.parse("""{"foo":"bar"}""")))
        status(result) shouldBe 400
      }

    }

  }

}
