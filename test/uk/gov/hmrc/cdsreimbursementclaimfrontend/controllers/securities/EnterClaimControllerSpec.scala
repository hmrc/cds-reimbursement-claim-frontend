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
import play.api.http.Status.NOT_FOUND
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.status
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
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
import scala.jdk.CollectionConverters._

class EnterClaimControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
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

    val caption = doc.select("span.govuk-caption-xl").eachText().asScala.toList
    val heading = doc.select(".govuk-heading-xl").eachText().asScala.toList

    caption should ===(
      List(
        messages(
          "enter-claim.securities.title.caption",
          securityDepositId
        )
      )
    )
    heading should ===(
      List(
        s"Security deposit: $securityDepositId Claim details for $taxCode - ${messages(s"select-duties.duty.$taxCode")}"
      )
    )

    doc.select("#amount-paid").text()                                       shouldBe amount.toPoundSterlingString
    doc.select("input[name='enter-claim.securities.claim-amount']").`val`() shouldBe ""
    doc.select("form").attr("action")                                       shouldBe routes.EnterClaimController.submit(securityDepositId, taxCode).url
  }

  "EnterClaimController" when {

    "show page for the first duty selected" must {

      def performAction(id: String): Future[Result] =
        controller.showFirst(id)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("foo")) shouldBe NOT_FOUND
      }

      "redirect to the first enter claim page if a valid security deposit ID with selected duties" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        for (depositId <- initialJourney.getSelectedDepositIds) {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          val firstDutySelected =
            initialJourney.getSelectedDutiesFor(depositId).flatMap(_.headOption).get

          checkIsRedirect(
            performAction(depositId),
            routes.EnterClaimController.show(depositId, firstDutySelected)
          )
        }
      }

      "redirect to the select duties page if a valid security deposit ID but no duties selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyWithDutiesPartiallySelected
        )
      ) { case (initialJourney, _) =>
        val depositIdsWithNoneDutiesSelected: Set[String] =
          initialJourney.answers.correctedAmounts.map(_.filter(_._2.isEmpty)).get.keySet

        depositIdsWithNoneDutiesSelected.nonEmpty shouldBe true

        for (depositId <- depositIdsWithNoneDutiesSelected) {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsRedirect(
            performAction(depositId),
            routes.ConfirmFullRepaymentController.show(depositId)
          )
        }
      }
    }

    "show page" must {

      def performAction(id: String, taxCode: TaxCode): Future[Result] = controller.show(id, taxCode)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("foo", TaxCode.A00)) shouldBe NOT_FOUND
      }

      "display the page if a valid security deposit ID" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
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
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
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
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {

          val selectedDuties  = initialJourney.getSelectedDutiesFor(depositId).getOrElse(Seq.empty)
          val availableDuties = initialJourney.getSecurityTaxCodesFor(depositId)

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
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {

          val availableDuties = initialJourney.getSecurityTaxCodesFor(depositId)
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

    "submit reclaim amount" must {

      def performAction(id: String, taxCode: TaxCode, data: (String, String)*): Future[Result] =
        controller.submit(id, taxCode)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("foo", TaxCode.A00)) shouldBe NOT_FOUND
      }

      "display error page if an invalid security deposit ID" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
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
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {

          val selectedDuties  = initialJourney.getSelectedDutiesFor(depositId).getOrElse(Seq.empty)
          val availableDuties = initialJourney.getSecurityTaxCodesFor(depositId)

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
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, (_, _, _, reclaims)) =>
        for ((depositId, taxCode, _) <- reclaims) {

          val availableDuties = initialJourney.getSecurityTaxCodesFor(depositId)
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

      "save reclaim amount and progress to the next page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        val allSelectedDuties: Seq[(String, TaxCode)] =
          initialJourney.getAllSelectedDuties

        for ((depositId, taxCode) <- allSelectedDuties) {

          val next = allSelectedDuties.nextAfter((depositId, taxCode))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(
              SessionData(
                initialJourney.submitCorrectAmount(depositId, taxCode, BigDecimal("0.01")).getOrFail
              )
            )(Right(()))

          }

          val expectedNextRoute: Call = next match {
            case None =>
              routes.CheckClaimDetailsController.show()

            case Some((secondDepositId, secondTaxCode)) =>
              if (secondDepositId == depositId)
                routes.EnterClaimController.show(depositId, secondTaxCode)
              else
                routes.ConfirmFullRepaymentController.show(secondDepositId)
          }

          checkIsRedirect(
            performAction(depositId, taxCode, "enter-claim.securities.claim-amount" -> "0.01"),
            expectedNextRoute
          )
        }

      }

      "save reclaim amount in change mode and progress to the next page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        val allSelectedDuties: Seq[(String, TaxCode)] =
          initialJourney.getAllSelectedDuties

        for ((depositId, taxCode) <- allSelectedDuties) {

          val updatedJourney = initialJourney
            .submitCheckClaimDetailsChangeMode(true)
            .submitCorrectAmount(depositId, taxCode, BigDecimal("0.01"))
            .getOrFail

          val next: Option[Either[String, (String, TaxCode)]] = updatedJourney.getNextDepositIdAndTaxCodeToClaim

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney.submitCheckClaimDetailsChangeMode(true)))
            mockStoreSession(
              SessionData(
                updatedJourney
              )
            )(Right(()))

          }

          val expectedNextRoute: Call = next match {
            case Some(Left(depositId)) =>
              routes.ConfirmFullRepaymentController.show(depositId)

            case Some(Right((depositId, taxCode))) =>
              routes.EnterClaimController.show(depositId, taxCode)

            case None =>
              routes.CheckClaimDetailsController.show()
          }

          checkIsRedirect(
            performAction(depositId, taxCode, "enter-claim.securities.claim-amount" -> "0.01"),
            expectedNextRoute
          )
        }

      }

      "re-display the page with error message if claimed amount is full amount" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        for ((depositId, taxCode) <- initialJourney.getAllSelectedDuties) {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          val amount = initialJourney.getSecurityDepositAmountFor(depositId, taxCode)

          checkPageWithErrorIsDisplayed(
            performAction(depositId, taxCode, "enter-claim.securities.claim-amount" -> amount.get.doubleValue.toString),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "Claim amount must be lower than or equal to the amount paid"
          )
        }
      }

      "re-display the page with error message if claimed amount is empty" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        for ((depositId, taxCode) <- initialJourney.getAllSelectedDuties) {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkPageWithErrorIsDisplayed(
            performAction(depositId, taxCode, "enter-claim.securities.claim-amount" -> ""),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "Enter the claim amount, in pounds"
          )
        }
      }

      "re-display the page with error message if claimed amount is greater then the deposit amount" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        for ((depositId, taxCode) <- initialJourney.getAllSelectedDuties) {

          val claimAmount =
            initialJourney.getSecurityDepositAmountFor(depositId, taxCode).get + BigDecimal("0.01")

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              depositId,
              taxCode,
              "enter-claim.securities.claim-amount" -> claimAmount.doubleValue.toString()
            ),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "Claim amount must be lower than or equal to the amount paid"
          )
        }
      }

      "re-display the page with error message if claimed amount is not a number" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney, _) =>
        for ((depositId, taxCode) <- initialJourney.getAllSelectedDuties) {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              depositId,
              taxCode,
              "enter-claim.securities.claim-amount" -> "2+1"
            ),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "Claim amount must be a number, like 30"
          )
        }
      }

      "redirect back to the CYA page when amount has not changed" in forAll(completeJourneyGen) { initialJourney =>
        initialJourney.getSecuritiesReclaims.foreachEntry { case (depositId, reclaims) =>
          reclaims.foreachEntry { case (taxCode, claimAmount) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
            }

            checkIsRedirect(
              performAction(
                depositId,
                taxCode,
                "enter-claim.securities.claim-amount" -> claimAmount.doubleValue.toString()
              ),
              routes.CheckYourAnswersController.show()
            )
          }
        }
      }

      "redirect back to the check claim page when amount has changed" in forAll(completeJourneyGen) { initialJourney =>
        initialJourney.getSecuritiesReclaims.foreachEntry { case (depositId, reclaims) =>
          reclaims.foreachEntry { case (taxCode, claimAmount) =>
            whenever(claimAmount > BigDecimal("0.01")) {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(initialJourney))
                mockStoreSession(
                  SessionData(
                    initialJourney.submitCorrectAmount(depositId, taxCode, BigDecimal("0.01")).getOrFail
                  )
                )(Right(()))
              }

              checkIsRedirect(
                performAction(
                  depositId,
                  taxCode,
                  "enter-claim.securities.claim-amount" -> "0.01"
                ),
                routes.CheckClaimDetailsController.show()
              )
            }
          }
        }
      }

    }
  }

}
