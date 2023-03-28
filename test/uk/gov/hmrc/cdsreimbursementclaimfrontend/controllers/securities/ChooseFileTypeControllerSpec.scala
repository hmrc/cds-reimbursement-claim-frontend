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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.concurrent.Future

class ChooseFileTypeControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TestWithJourneyGenerator[SecuritiesJourney] {

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
    featureSwitch.enable(Feature.Securities)

  def validateChooseFileTypePage(doc: Document, journey: SecuritiesJourney) = {
    val expectedDocumentTypes = journey.getDocumentTypesIfRequired.get
      .map(dt => (messages(s"choose-file-type.file-type.$dt"), UploadDocumentType.keyOf(dt)))

    radioItems(doc) should contain theSameElementsAs expectedDocumentTypes
    hasContinueButton(doc)
  }

  "ChooseFileTypeController" when {

    "Show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.securities.title"),
          doc => validateChooseFileTypePage(doc, journey)
        )

      }

      "display the page when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          whenever(journey.needsDocumentTypeSelection) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"$messagesKey.securities.title"),
              doc => validateChooseFileTypePage(doc, journey)
            )
          }
        }
      }
    }

    "submitted the document type" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not succeed if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction(("choose-file-type", "Other"))) shouldBe NOT_FOUND
      }

      "redirect to choose files when valid document type selection" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        journey.getDocumentTypesIfRequired.get.foreach { documentType =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(
              SessionData(journey.submitDocumentTypeSelection(documentType).getOrFail)
            )(Right(()))
          }
          checkIsRedirect(
            performAction("choose-file-type" -> s"$documentType"),
            routes.UploadFilesController.show()
          )
        }
      }

      "re-display the page when invalid document type selection" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }
        checkPageIsDisplayed(
          performAction("choose-file-type" -> "Foo"),
          messageFromMessageKey(s"$messagesKey.securities.title"),
          doc => validateChooseFileTypePage(doc, journey),
          expectedStatus = 400
        )
      }

      "re-display the page when unsupported document type selection" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }
        checkPageIsDisplayed(
          performAction("choose-file-type" -> "AirWayBill"),
          messageFromMessageKey(s"$messagesKey.securities.title"),
          doc => validateChooseFileTypePage(doc, journey),
          expectedStatus = 400
        )
      }

      "re-display the page when nothing has been selected" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsRequiringDocumentTypeWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.securities.title"),
          doc => validateChooseFileTypePage(doc, journey),
          expectedStatus = 400
        )
      }

      "redirect to choose files when valid document type selection when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          journey.getDocumentTypesIfRequired.get.foreach { documentType =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(
                SessionData(journey.submitDocumentTypeSelection(documentType).getOrFail)
              )(Right(()))
            }
            checkIsRedirect(
              performAction("choose-file-type" -> s"$documentType"),
              routes.UploadFilesController.show()
            )
          }
        }
      }

    }

  }

}
