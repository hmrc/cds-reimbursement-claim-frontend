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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import cats.data.EitherT
import org.jsoup.nodes.Document
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterExportMovementReferenceNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithJourneyGenerator[SecuritiesJourney]
    with Logging {

  val enterExportMovementReferenceNumberKey: String          = "enter-export-movement-reference-number"
  val enterExportMovementReferenceNumberKeyAndSubKey: String = s"$enterExportMovementReferenceNumberKey.securities"
  val mockClaimsService: ClaimService                        = mock[ClaimService]

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
    ()
  }

  val journey              = SecuritiesJourney.empty(exampleEori)
  val session: SessionData = SessionData(journey)

  def validateChooseExportMethodPage(doc: Document) = {
    val headerHtml     = doc.select(".govuk-heading-xl").html()
    val input          = doc.select("#enter-export-movement-reference-number")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    headerHtml          should ===(messages(s"$enterExportMovementReferenceNumberKeyAndSubKey.title"))
    input.attr("value") should ===("")
    continueButton      should ===(List(messages("button.continue")))
  }

  private def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  "Movement Reference Number Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if acc14 is present" in forAllWith(
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
          messageFromMessageKey(s"$enterExportMovementReferenceNumberKeyAndSubKey.title"),
          validateChooseExportMethodPage
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "save an export MRN if valid and continue to the check claimant details page" in forAllWith(
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
            SessionData(journey.submitExportMovementReferenceNumber(exampleMrn).getOrFail)
          )(Right(()))
        }

        checkIsRedirect(
          performAction(enterExportMovementReferenceNumberKey -> exampleMrnAsString),
          routes.CheckClaimantDetailsController.show()
        )
      }

      "display error if acc14 returns a successful response" in forAllWith(
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
          performAction(enterExportMovementReferenceNumberKey -> exampleMrnAsString),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberKeyAndSubKey.error.import"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid MRN" in forAllWith(
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
          performAction(enterExportMovementReferenceNumberKey -> invalidMRN.value),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberKeyAndSubKey.invalid.number"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty MRN" in forAllWith(
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
          performAction(enterExportMovementReferenceNumberKey -> ""),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterExportMovementReferenceNumberKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

    }
  }
}
