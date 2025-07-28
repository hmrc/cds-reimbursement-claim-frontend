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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class UploadBillOfDischarge4ControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TestWithJourneyGenerator[SecuritiesJourney] {

  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  val expectedUploadDocumentsLocation: String = "http://foo:123/bar/upload-bill-of-discharge4"

  def mockInitializeCall(existingFiles: Seq[UploadedFile] = Seq.empty) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where[UploadDocumentsConnector.Request, HeaderCarrier] { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFiles.map(_.upscanReference)
      })
      .returning(Future.successful(Some(expectedUploadDocumentsLocation)))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: UploadBillOfDischarge4Controller = instanceOf[UploadBillOfDischarge4Controller]

  "UploadBillOfDischarge4Controller" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "redirect to 'Bill of discharge (BOD) form' when no file uploaded yet" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyENUGen,
          journeyBuilder = buildSecuritiesJourneyReadyForENU
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockInitializeCall()
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to 'Bill of discharge (BOD) form' when some file uploaded already" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyENUGen,
          journeyBuilder = buildSecuritiesJourneyReadyForENU
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(journey.receiveBillOfDischargeDocuments(journey.answers.nonce, exampleUploadedFiles).getOrFail)
          )
          mockInitializeCall(exampleUploadedFiles)
        }

        checkIsRedirect(
          performAction(),
          Call("GET", s"$expectedUploadDocumentsLocation")
        )
      }

      "redirect to 'Bill of discharge (BOD) form' if journey has complete answers" in {
        forAll(completeJourneyOnlyENUGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockInitializeCall(journey.answers.billOfDischargeDocuments)
          }

          checkIsRedirect(
            performAction(),
            Call("GET", s"$expectedUploadDocumentsLocation")
          )
        }
      }

    }

    "'Bill of discharge (BOD) form' submitted the callback" must {

      def performAction(callback: UploadMrnListCallback): Future[Result] =
        controller.submit(FakeRequest().withJsonBody(Json.toJson(callback)))

      val callbackPayload: UploadMrnListCallback =
        UploadMrnListCallback(
          nonce = Nonce.random,
          uploadedFiles = exampleUploadedFiles
        )

      "return 204 if callback accepted" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyENUGen,
          journeyBuilder = buildSecuritiesJourneyReadyForENU
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(
              journey
                .receiveBillOfDischargeDocuments(journey.answers.nonce, exampleUploadedFiles)
                .getOrFail
            )
          )(Right(()))
        }
        val result = performAction(callbackPayload.copy(nonce = journey.answers.nonce))
        status(result) shouldBe 204
      }

      "return 400 if callback rejected because of invalid nonce" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyENUGen,
          journeyBuilder = buildSecuritiesJourneyReadyForENU
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(journey)
          )
        }
        val result = performAction(callbackPayload.copy(nonce = Nonce.random))
        status(result) shouldBe 400
      }

      "return 400 if callback rejected because of invalid request" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationOnlyENUGen,
          journeyBuilder = buildSecuritiesJourneyReadyForENU
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(journey)
          )
        }
        val result = controller.submit(FakeRequest().withJsonBody(Json.parse("""{"foo":"bar"}""")))
        status(result) shouldBe 400
      }

    }

  }

}
