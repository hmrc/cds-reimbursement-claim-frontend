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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with SummaryMatchers
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

  private val messagesKey: String = "check-claim.multiple"

  def assertPageContent(
    doc: Document,
    claim: OverpaymentsMultipleClaim
  ): Unit = {
    validateClaimsTablesForMultiple(
      doc,
      claim.getReimbursementsWithCorrectAmounts,
      routes.EnterClaimController.show
    )

    validateCheckClaimTotal(
      doc,
      claim.getTotalReimbursementAmount.toPoundSterlingString
    )
  }

  val claimGen: Gen[(OverpaymentsMultipleClaim, Seq[MRN])] =
    incompleteClaimWithCompleteClaimsGen(5)

  val claimWithNoClaimsGen: Gen[OverpaymentsMultipleClaim] =
    claimGen.map((claim, _) =>
      OverpaymentsMultipleClaim
        .unsafeModifyAnswers(claim, answers => answers.copy(correctedAmounts = None))
    )

  val claimWithIncompleteClaimsGen: Gen[OverpaymentsMultipleClaim] =
    claimGen.map((claim, _) =>
      OverpaymentsMultipleClaim
        .unsafeModifyAnswers(
          claim,
          answers =>
            answers
              .copy(correctedAmounts = claim.answers.correctedAmounts.map(_.clearFirstOption))
        )
    )

  val claimWithIncompleteMrnsGen: Gen[OverpaymentsMultipleClaim] =
    claimGen.map((claim, _) =>
      OverpaymentsMultipleClaim
        .unsafeModifyAnswers(
          claim,
          answers =>
            answers
              .copy(movementReferenceNumbers = claim.answers.movementReferenceNumbers.map(_.take(1)))
        )
    )

  "CheckClaimDetailsController" when {

    "Show" must {
      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page" in {
        forAll(claimGen) { case (claim, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              s"$messagesKey.title"
            ),
            doc => assertPageContent(doc, claim)
          )
        }
      }

      "display the page in change mode" in {
        forAll(completeClaimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              s"$messagesKey.title"
            ),
            doc => assertPageContent(doc, claim)
          )
        }
      }

      "redirect to enter mrn page if incomplete MRNs" in
        forAll(claimWithIncompleteMrnsGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterMovementReferenceNumberController.showFirst
          )
        }

      "redirect to select duties page if no claims" in
        forAll(claimWithNoClaimsGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.showFirst
          )
        }

      "redirect to select duties page if claims are incomplete" in
        forAll(claimWithIncompleteClaimsGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.showFirst
          )
        }
    }
  }
}
