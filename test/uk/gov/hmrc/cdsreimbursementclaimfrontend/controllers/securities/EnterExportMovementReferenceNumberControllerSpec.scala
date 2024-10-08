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

import cats.data.EitherT
import org.jsoup.nodes.Document
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class EnterExportMovementReferenceNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithJourneyGenerator[SecuritiesJourney]
    with Logging {

  val enterExportMovementReferenceNumberSingleKey: String          = "enter-export-movement-reference-number"
  val enterExportMovementReferenceNumberSingleKeyAndSubKey: String =
    s"$enterExportMovementReferenceNumberSingleKey.securities"

  val enterExportMovementReferenceNumberMultipleKey: String          = "enter-export-movement-reference-number.multiple"
  val enterExportMovementReferenceNumberMultipleKeyAndSubKey: String =
    s"$enterExportMovementReferenceNumberMultipleKey.securities"

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[ClaimService].toInstance(mockClaimsService),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterExportMovementReferenceNumberController =
    instanceOf[EnterExportMovementReferenceNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  val journey: SecuritiesJourney = SecuritiesJourney.empty(exampleEori)
  val session: SessionData       = SessionData(journey)

  def validateChooseExportMethodSinglePage(doc: Document): Assertion = {
    val headerHtml     = doc.select(".govuk-heading-xl").html()
    val input          = doc.select(s"#$enterExportMovementReferenceNumberSingleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    headerHtml          should ===(messages(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"))
    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
  }

  def validateChooseExportMethodMultiplePage(doc: Document): Assertion = {
    val headerHtml     = doc.select(".govuk-heading-xl").html()
    val input          = doc.select(s"#$enterExportMovementReferenceNumberMultipleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    headerHtml          should ===(messages(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"))
    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
  }

  private def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  "Movement Reference Number Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if acc14 is present (single shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
          validateChooseExportMethodSinglePage
        )
      }

      "display the page if acc14 is present (multiple shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
          validateChooseExportMethodMultiplePage
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "save an export MRN if valid and continue to the check claimant details page (single shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, (_, _, _, _)) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Left(Error("")))
          mockStoreSession(
            SessionData(
              journey.withEnterContactDetailsMode(true).submitExportMovementReferenceNumber(exampleMrn).getOrFail
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction(enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString),
          routes.EnterContactDetailsController.show
        )
      }

      "save an export MRN if valid and continue to the check claimant details page (multiple shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, (_, _, _, _)) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Left(Error("")))
          mockStoreSession(
            SessionData(
              journey.withEnterContactDetailsMode(true).submitExportMovementReferenceNumber(exampleMrn).getOrFail
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction(enterExportMovementReferenceNumberMultipleKey -> exampleMrnAsString),
          routes.EnterContactDetailsController.show
        )
      }

      "reject an export MRN if duplicate of import MRN (single shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Left(Error("")))
        }

        checkPageIsDisplayed(
          performAction(
            enterExportMovementReferenceNumberSingleKey -> journey.answers.movementReferenceNumber.get.value
          ),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberSingleKey.securities.error.import"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an export MRN if duplicate of import MRN (multiple shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Left(Error("")))
        }

        checkPageIsDisplayed(
          performAction(
            enterExportMovementReferenceNumberMultipleKey -> journey.answers.movementReferenceNumber.get.value
          ),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberMultipleKey.securities.error.import"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "display error if acc14 returns a successful response (single shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, (_, _, acc14, _)) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(acc14)))
        }

        checkPageIsDisplayed(
          performAction(enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.error.import"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "display error if acc14 returns a successful response (multiple shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, (_, _, acc14, _)) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(acc14)))
        }

        checkPageIsDisplayed(
          performAction(enterExportMovementReferenceNumberMultipleKey -> exampleMrnAsString),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.error.import"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid MRN (single shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session    = SessionData(journey)
        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterExportMovementReferenceNumberSingleKey -> invalidMRN.value),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.invalid.number"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid MRN (multiple shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session    = SessionData(journey)
        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterExportMovementReferenceNumberMultipleKey -> invalidMRN.value),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.invalid.number"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty MRN (single shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterExportMovementReferenceNumberSingleKey -> ""),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberSingleKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty MRN (multiple shipment)" in forAllWith(
        JourneyGenerator(
          mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
          buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterExportMovementReferenceNumberMultipleKey -> ""),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberSingleKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

    }
  }
}
