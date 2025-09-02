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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ClaimsTableValidator {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  def assertPageContent(
    doc: Document,
    journey: OverpaymentsSingleJourney
  ): Unit = {

    validateClaimsTableForSingle(
      doc,
      toReimbursementWithCorrectAmount(journey.getReimbursements),
      routes.EnterClaimController.show
    )
    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(
        m("check-claim.selected-duties.question") -> journey.getSelectedDuties
          .getOrElse(Seq.empty)
          .map(taxCode => s"${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}")
          .mkString(" "),
        m("check-claim.table.total")              -> journey.getTotalReimbursementAmount.toPoundSterlingString
      )
    )

    doc
      .getElementsByClass("govuk-summary-list__actions")
      .get(0)
      .getElementsByClass("govuk-link")
      .attr("href")                                     shouldBe routes.CheckClaimDetailsController.redirectToSelectDuties.url
    doc.getElementsByClass("govuk-button").attr("href") shouldBe routes.CheckClaimDetailsController.continue.url

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

  val journeyWithNoClaimsGen: Gen[OverpaymentsSingleJourney] =
    journeyGen.map(journey =>
      OverpaymentsSingleJourney
        .unsafeModifyAnswers(journey, answers => answers.copy(correctedAmounts = None))
    )

  val journeyWithIncompleteClaimsGen: Gen[OverpaymentsSingleJourney] =
    journeyGen.map(journey =>
      OverpaymentsSingleJourney
        .unsafeModifyAnswers(
          journey,
          answers =>
            answers
              .copy(correctedAmounts = journey.answers.correctedAmounts.map(_.clearFirstOption))
        )
    )

  "Check Claim Details Controller" when {

    "Show check claim details page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim.title"),
            assertPageContent(_, journey)
          )
        }

      "display the page in the change mode" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim.title"),
            assertPageContent(_, journey)
          )
        }

      "redirect to enter claim page if no claims" in
        forAll(journeyWithNoClaimsGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterClaimController.showFirst
          )
        }

      "redirect to enter claim page if claims are incomplete" in
        forAll(journeyWithIncompleteClaimsGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterClaimController.showFirst
          )
        }
    }

    "redirectToSelectDuties" must {

      def performAction(): Future[Result] =
        controller.redirectToSelectDuties(FakeRequest())

      "redirect to select duties page" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.SelectDutiesController.show)
        }
    }

    "continue to next page" must {

      def performAction(): Future[Result] =
        controller.continue(FakeRequest())

      "redirect to choose payee type page for incomplete journey" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(false)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.ChoosePayeeTypeController.show)
        }

      "continue to cya page for complete journey" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(false)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }
    }
  }
}
