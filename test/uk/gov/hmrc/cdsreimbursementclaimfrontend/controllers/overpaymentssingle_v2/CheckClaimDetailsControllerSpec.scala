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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import org.jsoup.nodes.Document
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  def assertPageContent(
    doc: Document,
    journey: OverpaymentsSingleJourney
  ): Unit = {
    val mrn = journey.getLeadMovementReferenceNumber.get.value
    assertPageElementsByIdAndExpectedText(doc)(
      "check-claim-summary-help-text"     -> m("check-claim-summary.help-text"),
      s"check-claim-summary-section-$mrn" -> m("check-claim-summary.duty.label", mrn),
      "check-claim-summary-yes-no"        -> s"${m("check-claim-summary.are-duties-correct")} ${m("check-claim-summary.yes")} ${m("check-claim-summary.no")}"
    )
    summaryKeyValueList(doc)          should containOnlyPairsOf(
      journey.getReimbursementClaims.toSeq.map { case (taxCode, amount) =>
        (s"$taxCode - ${m(s"select-duties.duty.$taxCode")}", amount.toPoundSterlingString)
      } ++
        Seq(m("check-claim-summary.total") -> journey.getTotalReimbursementAmount.toPoundSterlingString)
    )
  }

  val journeyGen: Gen[OverpaymentsSingleJourney] =
    buildJourneyFromAnswersGen(
      buildAnswersGen(
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        reimbursementMethod = None,
        submitEvidence = false,
        checkYourAnswersChangeMode = false
      )
    )

  "Check Claim Details Controller" when {

    "Show check claim details page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim-summary.title"),
            assertPageContent(_, journey)
          )
        }

      "display the page in the change mode" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim-summary.title"),
            assertPageContent(_, journey)
          )
        }
    }

    "Submit Enter Claim  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "accept YES response and redirect to the next page" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("check-claim-summary" -> "true"),
            if (journey.isAllSelectedDutiesAreCMAEligible)
              routes.ReimbursementMethodController.show
            else
              routes.CheckBankDetailsController.show
          )
        }

      "accept YES response and redirect to the CYA page when in change mode" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("check-claim-summary" -> "true"),
            routes.CheckYourAnswersController.show
          )
        }

      "accept NO response and redirect to select duties page" in {
        val journey = journeyGen.sample.get
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
        }

        checkIsRedirect(
          performAction("check-claim-summary" -> "false"),
          routes.SelectDutiesController.show
        )
      }
    }
  }
}
