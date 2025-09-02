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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import org.scalacheck.Gen
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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

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

  private val messagesKey: String = "check-claim.rejected-goods"

  def assertPageContent(
    doc: Document,
    journey: RejectedGoodsMultipleJourney
  ): Unit = {

    validateClaimsTablesForMultiple(
      doc,
      journey.getReimbursementsWithCorrectAmounts,
      routes.EnterClaimController.show
    )

    validateCheckClaimTotal(
      doc,
      journey.getTotalReimbursementAmount.toPoundSterlingString
    )
  }

  val journeyGen: Gen[(RejectedGoodsMultipleJourney, Seq[MRN])] =
    incompleteJourneyWithCompleteClaimsGen(5)

  val journeyWithNoClaimsGen: Gen[RejectedGoodsMultipleJourney] =
    journeyGen.map((journey, _) =>
      RejectedGoodsMultipleJourney
        .unsafeModifyAnswers(journey, answers => answers.copy(correctedAmounts = None))
    )

  val journeyWithIncompleteClaimsGen: Gen[RejectedGoodsMultipleJourney] =
    journeyGen.map((journey, _) =>
      RejectedGoodsMultipleJourney
        .unsafeModifyAnswers(
          journey,
          answers =>
            answers
              .copy(correctedAmounts = journey.answers.correctedAmounts.map(_.clearFirstOption))
        )
    )

  val journeyWithIncompleteMrnsGen: Gen[RejectedGoodsMultipleJourney] =
    journeyGen.map((journey, _) =>
      RejectedGoodsMultipleJourney
        .unsafeModifyAnswers(
          journey,
          answers =>
            answers
              .copy(movementReferenceNumbers = journey.answers.movementReferenceNumbers.map(_.take(1)))
        )
    )

  "CheckClaimDetailsController" when {

    "Show claim summary" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page if all selected claims provided" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(9)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title"),
            doc => assertPageContent(doc, journey)
          )
        }
      }

      "display the page in change mode" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title"),
            doc => assertPageContent(doc, journey)
          )
        }
      }

      "redirect to enter mrn page if incomplete MRNs" in
        forAll(journeyWithIncompleteMrnsGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterMovementReferenceNumberController.showFirst()
          )
        }

      "redirect to select duties page if no claims" in
        forAll(journeyWithNoClaimsGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.showFirst
          )
        }

      "redirect to select duties page if claims are incomplete" in
        forAll(journeyWithIncompleteClaimsGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.showFirst
          )
        }
    }
  }
}
