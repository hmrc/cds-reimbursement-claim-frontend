/*
 * Copyright 2022 HM Revenue & Customs
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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import play.api.test.Helpers.status
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.concurrent.Future

class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with TestWithJourneyGenerator[SecuritiesJourney] {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: EnterClaimController = instanceOf[EnterClaimController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "enter-claim.securities"

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  def validateEnterClaimPage(
    doc: Document,
    securityDepositId: String,
    taxCode: TaxCode,
    amount: BigDecimal
  ) = {
    doc.select("span.govuk-caption-xl").text()                              shouldBe messages(
      s"enter-claim.securities.title.caption",
      securityDepositId
    )
    doc.select("h1").text()                                                 shouldBe messages(
      "enter-claim.securities.title",
      taxCode,
      messages(s"select-duties.duty.$taxCode")
    )
    doc.select("#amount-paid").text()                                       shouldBe amount.toPoundSterlingString
    doc.select("input[name='enter-claim.securities.claim-amount']").`val`() shouldBe ""
    doc.select("form").attr("action")                                       shouldBe routes.EnterClaimController.submit(securityDepositId, taxCode).url
  }

  "EnterClaimController" when {

    "show page" must {

      def performAction(id: String, taxCode: TaxCode): Future[Result] = controller.show(id, taxCode)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("foo", TaxCode.A00)) shouldBe NOT_FOUND
      }

      "display the page if a valid security deposit ID" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          val depositAmount =
            initialJourney.getSecurityTaxDetailsFor(depositId, taxCode).map(_.getAmount).get

          checkPageIsDisplayed(
            performAction(depositId, taxCode),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            doc => validateEnterClaimPage(doc, depositId, taxCode, depositAmount)
          )
        }
      }

      "display error page if an invalid security deposit ID" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((_, taxCode, _) <- reclaims) {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsTechnicalErrorPage(
            performAction("dummyDepositId", taxCode)
          )
        }
      }

      "redirect back to the duties selection page if a valid deposit ID but the tax code was not selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {

          val selectedDuties  = initialJourney.getSelectedDutiesFor(depositId).getOrElse(Seq.empty)
          val availableDuties = initialJourney.getAvailableDutiesFor(depositId).getOrElse(Seq.empty)

          val unselectedDuty = availableDuties.filterNot(selectedDuties.contains).head

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsRedirect(
            performAction(depositId, unselectedDuty),
            routes.SelectDutiesController.show(depositId)
          )
        }
      }

      "redirect back to the duties selection page if a valid deposit ID but unavailable tax code" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {

          val availableDuties = initialJourney.getAvailableDutiesFor(depositId).getOrElse(Seq.empty)
          val wrongDuty       = TaxCodes.allExcept(availableDuties.toSet).head

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsRedirect(
            performAction(depositId, wrongDuty),
            routes.SelectDutiesController.show(depositId)
          )
        }
      }
    }

  }

}
