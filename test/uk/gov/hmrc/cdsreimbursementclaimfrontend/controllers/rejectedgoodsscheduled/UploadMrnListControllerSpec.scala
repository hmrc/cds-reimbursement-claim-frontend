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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.scalatest.BeforeAndAfterEach
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Json
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class UploadMrnListControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  val expectedUploadDocumentsLocation: String = "http://foo:123/bar/upload-mrn-list"

  def mockInitializeCall(existingFile: Option[UploadedFile] = None) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where[UploadDocumentsConnector.Request, HeaderCarrier] { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFile.map(_.upscanReference).toList
      })
      .returning(Future.successful(Some(expectedUploadDocumentsLocation)))

  def mockInitializeCallNotReturningRedirectLocation(existingFile: Option[UploadedFile] = None) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where[UploadDocumentsConnector.Request, HeaderCarrier] { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFile.toList.map(_.upscanReference)
      })
      .returning(Future.successful(None))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: UploadMrnListController = instanceOf[UploadMrnListController]

  "UploadMrnListController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "redirect to 'Upload Mrn List' when no file uploaded yet" in {
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockInitializeCall()
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to 'Upload Mrn List' when no file uploaded yet and no redirect location returned" in {
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockInitializeCallNotReturningRedirectLocation()
        }

        checkIsRedirect(
          performAction(),
          Call("GET", "http://localhost:10110/upload-customs-documents")
        )
      }

      "redirect to 'Upload Mrn List' when some file uploaded already" in {
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(claim.receiveScheduledDocument(claim.answers.nonce, exampleUploadedFile).getOrFail)
          )
          mockInitializeCall(Some(exampleUploadedFile))
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to 'Upload Mrn List' if claim has complete answers" in {
        forAll(completeClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockInitializeCall(claim.answers.scheduledDocument)
          }

          checkIsRedirect(
            performAction(),
            Call("GET", s"$expectedUploadDocumentsLocation")
          )
        }
      }

    }

    "'Upload Mrn List' submitted the callback" must {

      def performAction(callback: UploadMrnListCallback): Future[Result] =
        controller.submit(FakeRequest().withJsonBody(Json.toJson(callback)))

      val callbackPayload: UploadMrnListCallback =
        UploadMrnListCallback(
          nonce = Nonce.random,
          uploadedFiles = Seq(exampleUploadedFile)
        )

      "return 204 if callback accepted" in {
        val claim  = RejectedGoodsScheduledClaim.empty(exampleEori)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            SessionData(
              claim
                .receiveScheduledDocument(claim.answers.nonce, exampleUploadedFile)
                .getOrFail
            )
          )(Right(()))
        }
        val result = performAction(callbackPayload.copy(nonce = claim.answers.nonce))
        status(result) shouldBe 204
      }

      "return 204 if callback accepted with empty file list" in {
        val claim  = {
          val j = RejectedGoodsScheduledClaim.empty(exampleEori)
          j.receiveScheduledDocument(j.answers.nonce, exampleUploadedFile).getOrFail
        }
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            SessionData(
              claim.removeScheduledDocument
            )
          )(Right(()))
        }
        val result = performAction(
          UploadMrnListCallback(
            nonce = claim.answers.nonce,
            uploadedFiles = Seq.empty
          )
        )
        status(result) shouldBe 204
      }

      "return 400 if callback rejected because of invalid nonce" in {
        val claim  = RejectedGoodsScheduledClaim.empty(exampleEori)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(claim)
          )
        }
        val result = performAction(callbackPayload.copy(nonce = Nonce.random))
        status(result) shouldBe 400
      }

      "return 400 if callback rejected because of invalid request" in {
        val claim  = RejectedGoodsScheduledClaim.empty(exampleEori)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(claim)
          )
        }
        val result = controller.submit(FakeRequest().withJsonBody(Json.parse("""{"foo":"bar"}""")))
        status(result) shouldBe 400
      }

    }

  }

}
