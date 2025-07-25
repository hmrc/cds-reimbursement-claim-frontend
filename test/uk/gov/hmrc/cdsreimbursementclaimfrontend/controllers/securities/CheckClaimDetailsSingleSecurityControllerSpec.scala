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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.List
import scala.concurrent.Future

class CheckClaimDetailsSingleSecurityControllerSpec
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

  val controller: CheckClaimDetailsSingleSecurityController = instanceOf[CheckClaimDetailsSingleSecurityController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-claim.securities.single"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Securities)

  def validateCheckClaimDetailsPage(
    doc: Document,
    journey: SecuritiesJourney
  ) = {

    val claims = journey.getReclaimWithAmounts

    val reclaimsWithAmountsAndSecurityId = claims.head
    val securityDepositId                = reclaimsWithAmountsAndSecurityId._1
    val reclaimsWithAmounts              = reclaimsWithAmountsAndSecurityId._2
    val availableDuties                  = journey.getSecurityTaxCodesWithAmounts(securityDepositId)

    // verify claiming full amounts summary
    val claimFullAmountDutiesElement = doc.getElementById("claim-full-amount-selected-duties")
    claimFullAmountDutiesElement
      .getElementsByClass("govuk-summary-list__key")
      .eachText
      .get(0) shouldBe "Do you want to claim back the full amount?"
    claimFullAmountDutiesElement
      .getElementsByClass("govuk-summary-list__value")
      .eachText()
      .get(0) shouldBe (if journey.isFullSecurityAmountClaimed(securityDepositId) then "Yes" else "No")
    val fullAmountChangeLink = claimFullAmountDutiesElement.getElementById(s"change-claim-full-amount")
    fullAmountChangeLink.text()       shouldBe s"Change whether you want to claim the full amount"
    fullAmountChangeLink.attr("href") shouldBe routes.ConfirmSingleDepositRepaymentController.show.url

    // verify duties selected summary
    if (availableDuties.size > 1) {
      claimFullAmountDutiesElement
        .getElementsByClass("govuk-summary-list__key")
        .eachText()
        .get(1) shouldBe "What do you want to claim?"
      claimFullAmountDutiesElement
        .getElementsByClass("govuk-summary-list__value")
        .eachText()
        .get(1) shouldBe reclaimsWithAmounts
        .map(reclaims => messageFromMessageKey(s"tax-code.${reclaims.taxCode.value}"))
        .toList
        .mkString(" ")
      val dutiesChangeLink = claimFullAmountDutiesElement.getElementById(s"change-selected-duties")
      dutiesChangeLink.text()       shouldBe s"Change the charges you want to claim"
      dutiesChangeLink.attr("href") shouldBe routes.SelectDutiesController.showFirst.url
    }

    validateClaimsTablesForSingleSecurities(
      doc,
      securityDepositId,
      reclaimsWithAmounts,
      routes.EnterClaimController.show
    )
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

      "redirect to choose payee type page if RFS is not NTAS" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen.withReasonForSecurity(
            ReasonForSecurity.MissingPreferenceCertificate
          ),
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

      "redirect to choose export method page if RFS is NTAS" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen.withReasonForSecurity(
            ReasonForSecurity.TemporaryAdmission2M
          ),
          journeyBuilder = buildSecuritiesJourneyWithClaimsEntered
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney.submitCheckClaimDetailsChangeMode(true)))
        }

        checkIsRedirect(
          performAction(),
          routes.ChooseExportMethodController.show
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
