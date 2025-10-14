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
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithClaimGenerator

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class EnterClaimControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TestWithClaimGenerator[SecuritiesClaim] {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: EnterClaimController = instanceOf[EnterClaimController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "enter-claim.securities"

  def validateEnterClaimPage(
    doc: Document,
    securityDepositId: String,
    singleSecurity: Boolean,
    taxCode: TaxCode,
    amount: BigDecimal
  ) = {

    val caption         = doc.select("span.govuk-caption-l").eachText().asScala.toList
    val heading         = doc.select(".govuk-heading-l").eachText().asScala.toList
    val expectedCaption = messages("enter-claim.securities.securityIdLabel", securityDepositId)

    caption should ===(
      if singleSecurity then List.empty
      else List(expectedCaption)
    )
    heading should ===(
      if singleSecurity then List(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}")
      else List(s"$expectedCaption $taxCode - ${messages(s"select-duties.duty.$taxCode")}")
    )

    doc.select(".govuk-summary-list__row dd.govuk-summary-list__value").text() shouldBe amount.toPoundSterlingString
    doc.select("input[name='enter-claim-amount']").`val`()                     shouldBe ""
    doc.select("form").attr("action")                                          shouldBe routes.EnterClaimController.submit(securityDepositId, taxCode).url
  }

  "EnterClaimController" when {

    "show page for the first duty selected" must {

      def performAction(id: String): Future[Result] =
        controller.showFirst(id)(FakeRequest())

      "redirect to the first enter claim page if a valid security deposit ID with selected duties" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        for depositId <- initialClaim.getSelectedDepositIds do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          val firstDutySelected =
            initialClaim.getSelectedDutiesFor(depositId).flatMap(_.headOption).get

          checkIsRedirect(
            performAction(depositId),
            routes.EnterClaimController.show(depositId, firstDutySelected)
          )
        }
      }

      "redirect to the select duties page if a valid security deposit ID but no duties selected" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimWithDutiesPartiallySelected
        )
      ) { case (initialClaim, _) =>
        val depositIdsWithNoneDutiesSelected: Set[String] =
          initialClaim.answers.correctedAmounts.map(_.filter(_._2.isEmpty)).get.keySet

        depositIdsWithNoneDutiesSelected.nonEmpty shouldBe true

        for depositId <- depositIdsWithNoneDutiesSelected do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
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

      "display the page if a valid security deposit ID" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (depositId, taxCode, _, _) <- reclaims do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          val depositAmount =
            initialClaim.getSecurityTaxDetailsFor(depositId, taxCode).map(_.getAmount).get

          checkPageIsDisplayed(
            performAction(depositId, taxCode),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            doc =>
              validateEnterClaimPage(
                doc,
                depositId,
                initialClaim.isSingleSecurity,
                taxCode,
                depositAmount
              )
          )
        }
      }

      "display error page if an invalid security deposit ID" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (_, taxCode, _, _) <- reclaims do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsTechnicalErrorPage(
            performAction("dummyDepositId", taxCode)
          )
        }
      }

      "redirect back to the duties selection page if a valid deposit ID but the tax code was not selected" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (depositId, _, _, _) <- reclaims do {

          val selectedDuties  = initialClaim.getSelectedDutiesFor(depositId).getOrElse(Seq.empty)
          val availableDuties = initialClaim.getSecurityTaxCodesFor(depositId)

          availableDuties
            .filterNot(selectedDuties.contains)
            .foreach { unselectedDuty =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(initialClaim))
              }

              checkIsRedirect(
                performAction(depositId, unselectedDuty),
                routes.SelectDutiesController.show(depositId)
              )
            }
        }
      }

      "redirect back to the duties selection page if a valid deposit ID but unavailable tax code" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (depositId, _, _, _) <- reclaims do {

          val availableDuties = initialClaim.getSecurityTaxCodesFor(depositId)
          val wrongDuty       = TaxCodes.allExcept(availableDuties.toSet).head

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            performAction(depositId, wrongDuty),
            routes.SelectDutiesController.show(depositId)
          )
        }
      }
    }

    "submit claim amount" must {

      def performAction(id: String, taxCode: TaxCode, data: (String, String)*): Future[Result] =
        controller.submit(id, taxCode)(FakeRequest().withFormUrlEncodedBody(data*))

      "display error page if an invalid security deposit ID" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (_, taxCode, _, _) <- reclaims do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsTechnicalErrorPage(
            performAction("dummyDepositId", taxCode)
          )
        }
      }

      "redirect back to the duties selection page if a valid deposit ID but the tax code was not selected" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (depositId, _, _, _) <- reclaims do {

          val selectedDuties  = initialClaim.getSelectedDutiesFor(depositId).getOrElse(Seq.empty)
          val availableDuties = initialClaim.getSecurityTaxCodesFor(depositId)

          val unselectedDuty = availableDuties.filterNot(selectedDuties.contains).head

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            performAction(depositId, unselectedDuty),
            routes.SelectDutiesController.show(depositId)
          )
        }
      }

      "redirect back to the duties selection page if a valid deposit ID but unavailable tax code" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, (_, _, _, reclaims)) =>
        for (depositId, _, _, _) <- reclaims do {

          val availableDuties = initialClaim.getSecurityTaxCodesFor(depositId)
          val wrongDuty       = TaxCodes.allExcept(availableDuties.toSet).head

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            performAction(depositId, wrongDuty),
            routes.SelectDutiesController.show(depositId)
          )
        }
      }

      "save claim amount and progress to the next page" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        val allSelectedDuties: Seq[(String, TaxCode)] =
          initialClaim.getAllSelectedDuties

        for (depositId, taxCode) <- allSelectedDuties do {

          val next = allSelectedDuties.nextAfter((depositId, taxCode))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(
              SessionData(
                initialClaim.submitClaimAmount(depositId, taxCode, BigDecimal("0.01")).getOrFail
              )
            )(Right(()))

          }

          val expectedNextRoute: Call = next match {
            case None =>
              routes.CheckClaimDetailsController.show

            case Some((secondDepositId, secondTaxCode)) =>
              if secondDepositId == depositId then routes.EnterClaimController.show(depositId, secondTaxCode)
              else routes.ConfirmFullRepaymentController.show(secondDepositId)
          }

          checkIsRedirect(
            performAction(depositId, taxCode, "enter-claim-amount" -> "0.01"),
            expectedNextRoute
          )
        }

      }

      "save claim amount and progress to the next page when single security" in {
        forAllWith(
          ClaimGenerator(
            testParamsGenerator = SecuritiesSingleClaimGenerators.mrnWithRfsWithImportDeclarationWithReclaimsGen,
            claimBuilder = SecuritiesSingleClaimGenerators.buildSecuritiesClaimReadyForEnteringClaimAmounts
          )
        ) { case (initialClaim, _) =>
          val allSelectedDuties: Seq[(String, TaxCode)] =
            initialClaim.getAllSelectedDuties

          for (depositId, taxCode) <- allSelectedDuties do {

            val next = allSelectedDuties.nextAfter((depositId, taxCode))

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(
                SessionData(
                  initialClaim.submitClaimAmount(depositId, taxCode, BigDecimal("0.01")).getOrFail
                )
              )(Right(()))

            }

            val expectedNextRoute: Call = next match {
              case None                     =>
                routes.CheckClaimDetailsSingleSecurityController.show
              case Some((_, secondTaxCode)) =>
                routes.EnterClaimController.show(depositId, secondTaxCode)
            }

            checkIsRedirect(
              performAction(depositId, taxCode, "enter-claim-amount" -> "0.01"),
              expectedNextRoute
            )
          }
        }

      }

      "save claim amount in change mode and progress to the next page" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        val allSelectedDuties: Seq[(String, TaxCode)] =
          initialClaim.getAllSelectedDuties

        for (depositId, taxCode) <- allSelectedDuties do {

          val updatedClaim = initialClaim
            .submitCheckClaimDetailsChangeMode(true)
            .submitClaimAmount(depositId, taxCode, BigDecimal("0.01"))
            .getOrFail

          val next: Option[Either[String, (String, TaxCode)]] = updatedClaim.getNextDepositIdAndTaxCodeToClaim

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim.submitCheckClaimDetailsChangeMode(true)))
            mockStoreSession(
              SessionData(
                updatedClaim
              )
            )(Right(()))

          }

          val expectedNextRoute: Call = next match {
            case Some(Left(id)) =>
              routes.ConfirmFullRepaymentController.show(id)

            case Some(Right((id, code))) =>
              routes.EnterClaimController.show(id, code)

            case None =>
              routes.CheckClaimDetailsController.show
          }

          checkIsRedirect(
            performAction(depositId, taxCode, "enter-claim-amount" -> "0.01"),
            expectedNextRoute
          )
        }

      }

      "re-display the page with error message if claimed amount is greater then the deposit amount" in forSomeWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        for (depositId, taxCode) <- initialClaim.getAllSelectedDuties do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          val amount = initialClaim.getSecurityDepositAmountFor(depositId, taxCode)

          checkPageWithErrorIsDisplayed(
            performAction(
              depositId,
              taxCode,
              "enter-claim-amount" -> (amount.get + BigDecimal("0.01")).doubleValue.toString()
            ),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "The amount that you want to claim back must be more than zero and less or equal to the amount paid"
          )
        }
      }

      "re-display the page with error message if claimed amount is empty" in forSomeWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        for (depositId, taxCode) <- initialClaim.getAllSelectedDuties do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageWithErrorIsDisplayed(
            performAction(depositId, taxCode, "enter-claim-amount" -> ""),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "Enter the amount you want to claim back, in pounds"
          )
        }
      }

      "re-display the page with error message if claimed amount is zero" in forSomeWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        for (depositId, taxCode) <- initialClaim.getAllSelectedDuties do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              depositId,
              taxCode,
              "enter-claim-amount" -> "0"
            ),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "The amount that you want to claim back must be more than zero and less or equal to the amount paid"
          )
        }
      }

      "re-display the page with error message if claimed amount is not a number" in forSomeWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationWithReclaimsGen,
          claimBuilder = buildSecuritiesClaimReadyForEnteringClaimAmounts
        )
      ) { case (initialClaim, _) =>
        for (depositId, taxCode) <- initialClaim.getAllSelectedDuties do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageWithErrorIsDisplayed(
            performAction(
              depositId,
              taxCode,
              "enter-claim-amount" -> "2+1"
            ),
            messageFromMessageKey(s"$messagesKey.title", taxCode, messages(s"select-duties.duty.$taxCode")),
            "The amount that you want to claim back must be a number, like 30 or 60.55"
          )
        }
      }

      "redirect back to the CYA page when amount has not changed" in forAll(completeClaimGen) { initialClaim =>
        initialClaim.getSecuritiesReclaims.foreachEntry { case (depositId, reclaims) =>
          reclaims.foreachEntry { case (taxCode, claimAmount) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkIsRedirect(
              performAction(
                depositId,
                taxCode,
                "enter-claim-amount" -> claimAmount.doubleValue.toString()
              ),
              routes.CheckYourAnswersController.show
            )
          }
        }
      }

      "redirect back to the check claim page when amount has changed" in forAll(completeClaimGen) { initialClaim =>
        initialClaim.getSecuritiesReclaims.foreachEntry { case (depositId, reclaims) =>
          reclaims.foreachEntry { case (taxCode, claimAmount) =>
            whenever(claimAmount > BigDecimal("0.01")) {
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(initialClaim))
                mockStoreSession(
                  SessionData(
                    initialClaim.submitClaimAmount(depositId, taxCode, BigDecimal("0.01")).getOrFail
                  )
                )(Right(()))
              }

              checkIsRedirect(
                performAction(
                  depositId,
                  taxCode,
                  "enter-claim-amount" -> "0.01"
                ),
                routes.CheckClaimDetailsController.show
              )
            }
          }
        }
      }

    }
  }

}
