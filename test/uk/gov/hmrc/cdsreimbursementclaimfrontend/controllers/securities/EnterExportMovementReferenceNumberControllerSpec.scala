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
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.EnterExportMovementReferenceNumberHelper

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

  val enterExportMovementReferenceNumberMultipleKey: String          = "enter-export-movement-reference-number.next"
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

  def validateChooseExportMethodFirstPage(doc: Document): Assertion = {
    val headerHtml     = doc.select(".govuk-label--xl").html()
    val input          = doc.select(s"#$enterExportMovementReferenceNumberSingleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    headerHtml          should ===(messages(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"))
    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
  }

  def validateChooseExportMethodNextPage(doc: Document): Assertion = {
    val input          = doc.select(s"#$enterExportMovementReferenceNumberMultipleKey")
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    input.attr("value") should ===("")
    continueButton      should contain(messages("button.continue"))
  }

  "Movement Reference Number Controller" when {
    "Enter MRN page" must {

      def performAction(mrnIndex: Int): Future[Result] =
        if (mrnIndex === 0) controller.showFirst(FakeRequest())
        else controller.showNext(mrnIndex + 1)(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction(0)) shouldBe NOT_FOUND
      }

      "display the page if acc14 is present (first mrn)" in forAllWith(
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
          performAction(0),
          messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
          validateChooseExportMethodFirstPage
        )
      }

      "display the page if acc14 is present (next mrn)" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"))
          )
        )
      ) { case (journey, _) =>
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(1),
          EnterExportMovementReferenceNumberHelper.title(2),
          validateChooseExportMethodNextPage
        )
      }
    }

    "Submit MRN page" must {

      def performAction(mrnIndex: Int, data: (String, String)*): Future[Result] =
        if (mrnIndex === 0) controller.submitFirst(FakeRequest().withFormUrlEncodedBody(data: _*))
        else controller.submitNext(mrnIndex + 1)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction(0)) shouldBe NOT_FOUND
      }

      // "save an export MRN if valid and continue to the check claimant details page (first mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, (_, _, _, _)) =>
      //   val session = SessionData(journey)

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(Left(Error("")))
      //     mockStoreSession(
      //       SessionData(
      //         journey.withEnterContactDetailsMode(true).submitExportMovementReferenceNumber(0, exampleMrn).getOrFail
      //       )
      //     )(Right(()))
      //   }

      //   checkIsRedirect(
      //     performAction(0, enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString),
      //     routes.EnterContactDetailsController.show
      //   )
      // }

      // "save an export MRN if valid and continue to the check claimant details page (next mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, (_, _, _, _)) =>
      //   val session = SessionData(journey)

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(Left(Error("")))
      //     mockStoreSession(
      //       SessionData(
      //         journey.withEnterContactDetailsMode(true).submitExportMovementReferenceNumber(0, exampleMrn).getOrFail
      //       )
      //     )(Right(()))
      //   }

      //   checkIsRedirect(
      //     performAction(1, enterExportMovementReferenceNumberMultipleKey -> exampleMrnAsString),
      //     routes.EnterContactDetailsController.show
      //   )
      // }

      // "reject an export MRN if duplicate of import MRN (first mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, _) =>
      //   val session = SessionData(journey)

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(Left(Error("")))
      //   }

      //   checkPageIsDisplayed(
      //     performAction(
      //       0,
      //       enterExportMovementReferenceNumberSingleKey -> journey.answers.movementReferenceNumber.get.value
      //     ),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberSingleKey.securities.error.import"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      // "reject an export MRN if duplicate of import MRN (next mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, _) =>
      //   val session = SessionData(journey)

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(Left(Error("")))
      //   }

      //   checkPageIsDisplayed(
      //     performAction(
      //       1,
      //       enterExportMovementReferenceNumberMultipleKey -> journey.answers.movementReferenceNumber.get.value
      //     ),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberMultipleKey.securities.error.import"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      // "display error if acc14 returns a successful response (first mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, (_, _, acc14, _)) =>
      //   val session = SessionData(journey)

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(Right(Some(acc14)))
      //   }

      //   checkPageIsDisplayed(
      //     performAction(0, enterExportMovementReferenceNumberSingleKey -> exampleMrnAsString),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.error.import"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      // "display error if acc14 returns a successful response (next mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, (_, _, acc14, _)) =>
      //   val session = SessionData(journey)

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(Right(Some(acc14)))
      //   }

      //   checkPageIsDisplayed(
      //     performAction(1, enterExportMovementReferenceNumberMultipleKey -> exampleMrnAsString),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.error.import"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      // "reject an invalid MRN (first mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, _) =>
      //   val session    = SessionData(journey)
      //   val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //   }

      //   checkPageIsDisplayed(
      //     performAction(0, enterExportMovementReferenceNumberSingleKey -> invalidMRN.value),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.invalid.number"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      // "reject an invalid MRN (next mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, _) =>
      //   val session    = SessionData(journey)
      //   val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //   }

      //   checkPageIsDisplayed(
      //     performAction(1, enterExportMovementReferenceNumberMultipleKey -> invalidMRN.value),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.invalid.number"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      // "reject an empty MRN (first mrn)" in forAllWith(
      //   JourneyGenerator(
      //     mrnWithRfsTempAdmissionWithDisplayDeclarationWithSingleShipmentMfdGen,
      //     buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //   )
      // ) { case (journey, _) =>
      //   val session = SessionData(journey)
      //   inSequence {
      //     mockAuthWithNoRetrievals()
      //     mockGetSession(session)
      //   }

      //   checkPageIsDisplayed(
      //     performAction(0, enterExportMovementReferenceNumberSingleKey -> ""),
      //     messageFromMessageKey(s"$enterExportMovementReferenceNumberSingleKeyAndSubKey.title"),
      //     doc =>
      //       getErrorSummary(doc) shouldBe messageFromMessageKey(
      //         s"$enterExportMovementReferenceNumberSingleKey.error.required"
      //       ),
      //     expectedStatus = BAD_REQUEST
      //   )
      // }

      //   "reject an empty MRN (next mrn)" in forAllWith(
      //     JourneyGenerator(
      //       mrnWithRfsTempAdmissionWithDisplayDeclarationWithMultipleShipmentMfdGen,
      //       buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal
      //     )
      //   ) { case (journey, _) =>
      //     val session = SessionData(journey)
      //     inSequence {
      //       mockAuthWithNoRetrievals()
      //       mockGetSession(session)
      //     }

      //     checkPageIsDisplayed(
      //       performAction(1, enterExportMovementReferenceNumberMultipleKey -> ""),
      //       messageFromMessageKey(s"$enterExportMovementReferenceNumberMultipleKeyAndSubKey.title"),
      //       doc =>
      //         getErrorSummary(doc) shouldBe messageFromMessageKey(
      //           s"$enterExportMovementReferenceNumberSingleKey.error.required"
      //         ),
      //       expectedStatus = BAD_REQUEST
      //     )
      //   }

    }
  }
}
