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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators.*
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
    claim: OverpaymentsSingleClaim
  ): Unit = {

    validateClaimsTableForSingle(
      doc,
      toReimbursementWithCorrectAmount(claim.getReimbursements),
      routes.EnterClaimController.show
    )
    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(
        m("check-claim.selected-duties.question") -> claim.getSelectedDuties
          .getOrElse(Seq.empty)
          .map(taxCode => s"${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}")
          .mkString(" "),
        m("check-claim.table.total")              -> claim.getTotalReimbursementAmount.toPoundSterlingString
      )
    )

    doc
      .getElementsByClass("govuk-summary-list__actions")
      .get(0)
      .getElementsByClass("govuk-link")
      .attr("href")                                     shouldBe routes.CheckClaimDetailsController.redirectToSelectDuties.url
    doc.getElementsByClass("govuk-button").attr("href") shouldBe routes.CheckClaimDetailsController.continue.url

  }

  val claimGen: Gen[OverpaymentsSingleClaim] =
    buildClaimFromAnswersGen(
      buildAnswersGen(
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        reimbursementMethod = None,
        submitEvidence = false,
        checkYourAnswersChangeMode = false
      )
    )

  val claimWithNoClaimsGen: Gen[OverpaymentsSingleClaim] =
    claimGen.map(claim =>
      OverpaymentsSingleClaim
        .unsafeModifyAnswers(claim, answers => answers.copy(correctedAmounts = None))
    )

  val claimWithIncompleteClaimsGen: Gen[OverpaymentsSingleClaim] =
    claimGen.map(claim =>
      OverpaymentsSingleClaim
        .unsafeModifyAnswers(
          claim,
          answers =>
            answers
              .copy(correctedAmounts = claim.answers.correctedAmounts.map(_.clearFirstOption))
        )
    )

  "Check Claim Details Controller" when {

    "Show check claim details page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page" in
        forAll(claimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim.title"),
            assertPageContent(_, claim)
          )
        }

      "display the page in the change mode" in
        forAll(completeClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim.title"),
            assertPageContent(_, claim)
          )
        }

      "redirect to enter claim page if no claims" in
        forAll(claimWithNoClaimsGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterClaimController.showFirst
          )
        }

      "redirect to enter claim page if claims are incomplete" in
        forAll(claimWithIncompleteClaimsGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
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
        forAll(claimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(SessionData(claim.withDutiesChangeMode(true)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.SelectDutiesController.show)
        }
    }

    "continue to next page" must {

      def performAction(): Future[Result] =
        controller.continue(FakeRequest())

      "redirect to choose payee type page for incomplete claim" in
        forAll(claimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(SessionData(claim.withDutiesChangeMode(false)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.ChoosePayeeTypeController.show)
        }

      "continue to cya page for complete claim" in
        forAll(completeClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(SessionData(claim.withDutiesChangeMode(false)))(Right(()))
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }
    }
  }
}
