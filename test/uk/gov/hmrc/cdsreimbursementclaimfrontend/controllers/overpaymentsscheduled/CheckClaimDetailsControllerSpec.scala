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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.buildAnswersGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.buildJourneyFromAnswersGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
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
    journey: OverpaymentsScheduledJourney
  ): Unit = {
    val claims                       = ClaimsTableHelper.sortReimbursementsByDisplayDuty(journey.getReimbursements)
    val nonExciseDutyClaims          = journey.getNonExciseDutyClaims
    val selectedExciseCategoryClaims = journey.getSelectedExciseCategoryClaims
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
        m("check-claim.table.total")            -> journey.getTotalReimbursementAmount.toPoundSterlingString
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

  val journeyGen: Gen[OverpaymentsScheduledJourney] =
    buildJourneyFromAnswersGen(
      buildAnswersGen(
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        submitEvidence = false,
        checkYourAnswersChangeMode = false
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
            messageFromMessageKey("check-claim.scheduled.title"),
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
            messageFromMessageKey("check-claim.scheduled.title"),
            assertPageContent(_, journey)
          )
        }
    }

    "Submit Enter Claim  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data*)
        )

      "redirect to the next page" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.ChoosePayeeTypeController.show
          )
        }

      "redirect to the CYA page when in change mode" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.CheckYourAnswersController.show
          )
        }
    }
  }
}
