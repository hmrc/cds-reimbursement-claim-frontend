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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaimGenerators.buildAnswersGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaimGenerators.buildClaimFromAnswersGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaimGenerators.completeClaimGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimsTableHelper

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
    claim: OverpaymentsScheduledClaim
  ): Unit = {
    val claims                       = ClaimsTableHelper.sortReimbursementsByDisplayDuty(claim.getReimbursements)
    val nonExciseDutyClaims          = claim.getNonExciseDutyClaims
    val selectedExciseCategoryClaims = claim.getSelectedExciseCategoryClaims
    val selectedExciseCategories     = selectedExciseCategoryClaims.keys.map(_.repr).toList

    validateClaimsTablesForScheduled(
      doc,
      nonExciseDutyClaims,
      selectedExciseCategoryClaims,
      routes.EnterClaimController.show
    )

    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(
        m("check-claim.duty-types-summary.key") -> claims.keys
          .map(dutyType => m(s"select-duty-types.${dutyType.repr}"))
          .mkString(" "),
        m("check-claim.table.total")            -> claim.getTotalReimbursementAmount.toPoundSterlingString
      ) ++
        nonExciseDutyClaims.map { case (dutyType, claims) =>
          m(s"select-duty-codes.title.${dutyType.repr}") -> claims
            .map(claim => m(s"tax-code.${claim.taxCode}"))
            .mkString(" ")
        } ++
        Seq(
          m("select-duty-codes.title.excise-duty") -> selectedExciseCategories
            .map(category => m(s"excise-category.$category"))
            .mkString(" ")
        ).filter(_ => selectedExciseCategoryClaims.nonEmpty) ++
        selectedExciseCategoryClaims
          .map { case (category, claims) =>
            m(
              s"check-claim.duties-selected-summary.key",
              messages(s"select-excise-duty-codes.h1.${category.repr}")
            ) -> claims.map(claim => m(s"tax-code.${claim.taxCode}")).mkString(" ")
          }
          .filter(_ => selectedExciseCategoryClaims.nonEmpty)
    )
  }

  val claimGen: Gen[OverpaymentsScheduledClaim] =
    buildClaimFromAnswersGen(
      buildAnswersGen(
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        submitEvidence = false,
        checkYourAnswersChangeMode = false
      )
    )

  val incompleteClaimGen: Gen[OverpaymentsScheduledClaim] =
    buildClaimFromAnswersGen(
      buildAnswersGen(
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        submitEvidence = false,
        checkYourAnswersChangeMode = false
      ).map(answers => answers.copy(correctedAmounts = answers.correctedAmounts.map(_.clearFirstOption)))
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
            messageFromMessageKey("check-claim.scheduled.title"),
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
            messageFromMessageKey("check-claim.scheduled.title"),
            assertPageContent(_, claim)
          )
        }

      "redirect when reimbursements not completed yet" in
        forAll(incompleteClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutyTypesController.show
          )
        }
    }

    "Submit Enter Claim  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data*)
        )

      "redirect to the next page" in
        forAll(claimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.ChoosePayeeTypeController.show
          )
        }

      "redirect to the CYA page when in change mode" in
        forAll(completeClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.CheckYourAnswersController.show
          )
        }
    }
  }
}
