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
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.List
import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TestWithJourneyGenerator[SecuritiesJourney]
    with SummaryMatchers
    with ClaimsTableValidator {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-claim.securities"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Securities)

  def validateCheckClaimDetailsPage(
    doc: Document,
    journey: SecuritiesJourney
  ) = {

    val claims = journey.getReclaimWithAmounts

    // verify claiming full amounts
    claims.map { case (securityDepositId, reclaimsList) =>
      doc
        .getElementById(s"security-deposit-id-h2-$securityDepositId")
        .text() shouldBe s"Security deposit ID: $securityDepositId"

      reclaimsList.map { reclaim =>
        val claimFullAmountElement = doc.getElementById(s"claim-full-amount-$securityDepositId")
        claimFullAmountElement.getElementsByClass("govuk-summary-list__key").text() shouldBe "Claiming full amount?"
        claimFullAmountElement
          .getElementsByClass("govuk-summary-list__value")
          .text()                                                                   shouldBe (if journey.isFullSecurityAmountClaimed(securityDepositId) then "Yes" else "No")
        val changeLink = claimFullAmountElement.getElementById(s"change-claim-full-amount-$securityDepositId")
        changeLink.text()       shouldBe s"Change claim full amount for Security ID: $securityDepositId"
        changeLink.attr("href") shouldBe routes.ConfirmFullRepaymentController.show(securityDepositId).url
      }
    }

    validateClaimsTablesForSecurities(doc, claims, routes.EnterClaimController.show)
    val repaymentTotalElement = doc.getElementById("repayment-total")
    repaymentTotalElement.getElementsByClass("govuk-summary-list__key").text() shouldBe "Total claim amount"
    repaymentTotalElement
      .getElementsByClass("govuk-summary-list__value")
      .text()                                                                  shouldBe journey.getReclaimWithAmounts.values.flatten.map(_.claimAmount).sum.toPoundSterlingString

  }

  "CheckClaimDetailsController" when {
    "show page" must {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyWithClaimsEntered
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
          mockStoreSession(SessionData(initialJourney.submitCheckClaimDetailsChangeMode(true)))(Right(()))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateCheckClaimDetailsPage(doc, initialJourney)
        )

      }
    }

    "submit page" must {
      def performAction(): Future[Result] = controller.submit(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the next page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyWithClaimsEntered
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney.submitCheckClaimDetailsChangeMode(true)))
        }

        checkIsRedirect(
          performAction(),
          routes.ChoosePayeeTypeController.show
        )
      }

      "redirect to the CYA page when in change your answers mode" in forAll(completeJourneyGen) { initialJourney =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.CheckYourAnswersController.show
        )
      }
    }
  }
}
