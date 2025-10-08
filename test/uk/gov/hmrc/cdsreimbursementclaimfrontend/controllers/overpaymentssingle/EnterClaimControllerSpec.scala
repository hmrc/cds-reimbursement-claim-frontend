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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails

import scala.concurrent.Future

class EnterClaimControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterClaimController = instanceOf[EnterClaimController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  def assertPageContent(
    doc: Document,
    claim: OverpaymentsSingleClaim,
    taxCode: TaxCode,
    actualAmountOpt: Option[BigDecimal]
  ): Unit = {
    val paidAmount: BigDecimal =
      claim
        .getNdrcDetailsFor(taxCode)
        .map(_.amount)
        .map(BigDecimal.apply)
        .getOrElse(fail(s"Missing paid amount for $taxCode"))

    val claimAmountOpt = actualAmountOpt.map(a => paidAmount - a)

    assertPageElementsByIdAndExpectedHtml(doc)(
      "enter-claim-agent-fees-disclaimer" -> m("enter-claim.inset-text"),
      "enter-claim-how-much-was-paid"     ->
        (if TaxCodes.custom.contains(taxCode) then
           m(
             "enter-claim.paid-amount-label",
             paidAmount.toPoundSterlingString,
             taxCode,
             m(s"select-duties.duty.$taxCode")
           )
         else
           m(
             s"enter-claim.paid-amount-label.excise",
             paidAmount.toPoundSterlingString,
             messages(s"duty-type.${taxCode.dutyType.repr}"),
             taxCode.value
           )
        ),
      "enter-claim-amount-label"          -> m("enter-claim-amount.label")
    )

    assertPageInputsByIdAndExpectedValue(doc)(
      "enter-claim-amount" ->
        claimAmountOpt.fold("")(a => formatAmount(a))
    )
  }

  val claimGen: Gen[OverpaymentsSingleClaim] =
    buildClaimFromAnswersGen(answersWithDutiesSelectedGen())

  "Enter Claim Controller" when {

    "Show first enter claim page" must {

      def performAction(): Future[Result] =
        controller.showFirst()(FakeRequest())

      "display the page" in forAll(claimGen) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        val expectedTaxCode: TaxCode =
          claim.getSelectedDuties.get.head

        checkIsRedirect(
          performAction(),
          routes.EnterClaimController.show(expectedTaxCode)
        )
      }

      "display the page back in the change mode" in
        forAll(claimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          val expectedTaxCode: TaxCode =
            claim.getSelectedDuties.get.head

          checkIsRedirect(
            performAction(),
            routes.EnterClaimController.show(expectedTaxCode)
          )
        }

      "redirect to select duties page if no claims selected" in
        forAll(buildClaimFromAnswersGen(answersUpToBasisForClaimGen())) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.show
          )
        }

      "redirect to claims summary when claims are complete and not in change mode" in {
        val claim = completeClaimGen.sample.get.submitCheckYourAnswersChangeMode(false)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(),
          routes.CheckClaimDetailsController.show
        )
      }
    }

    "Show enter claim page by tax code" must {

      def performAction(taxCode: TaxCode): Future[Result] =
        controller.show(taxCode)(FakeRequest())

      "display the page" in forAll(claimGen) { claim =>
        claim.getSelectedDuties.get.foreach { taxCode =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(taxCode),
            if TaxCodes.custom.contains(taxCode) then
              messageFromMessageKey(
                "enter-claim.title",
                taxCode.value,
                messageFromMessageKey(s"select-duties.duty.$taxCode")
              )
            else
              messageFromMessageKey(
                "enter-claim.title.excise",
                messages(s"duty-type.${taxCode.dutyType.repr}"),
                taxCode.value,
                messages(s"excise-category.${taxCode.exciseCategory.map(_.repr).getOrElse("none")}")
              )
            ,
            assertPageContent(_, claim, taxCode, None)
          )
        }
      }

      "display the page back in the change mode" in
        forAll(completeClaimGen) { claim =>
          claim.getSelectedDuties.get.foreach { taxCode =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            val actualAmountOpt: Option[BigDecimal] =
              claim.answers.correctedAmounts.flatMap(_.get(taxCode).flatten)

            checkPageIsDisplayed(
              performAction(taxCode),
              if TaxCodes.custom.contains(taxCode) then
                messageFromMessageKey(
                  "enter-claim.title",
                  taxCode.value,
                  messageFromMessageKey(s"select-duties.duty.$taxCode")
                )
              else
                messageFromMessageKey(
                  "enter-claim.title.excise",
                  messages(s"duty-type.${taxCode.dutyType.repr}"),
                  taxCode.value,
                  messages(s"excise-category.${taxCode.exciseCategory.map(_.repr).getOrElse("none")}")
                )
              ,
              assertPageContent(_, claim, taxCode, actualAmountOpt)
            )
          }
        }

      "redirect to select duties page when no duties selected" in {
        val claim = OverpaymentsSingleClaim
          .tryBuildFrom(
            claimGen.sample.getOrElse(fail("Failed to create claim")).answers.copy(correctedAmounts = None)
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(TaxCode("A00")),
          routes.SelectDutiesController.show
        )
      }

      "when wrong code, redirect to select duties page" in
        forAll(claimGen) { claim =>
          val selected = claim.getSelectedDuties.get
          val stranger = TaxCodes.all.find(tc => !selected.contains(tc)).get
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(stranger),
            routes.SelectDutiesController.show
          )
        }

      "when wrong code, redirect to check claims if all amounts provided already" in
        forAll(buildClaimFromAnswersGen(answersWithAllAmountsProvidedGen())) { claim =>
          val selected = claim.getSelectedDuties.get
          val stranger = TaxCodes.all.find(tc => !selected.contains(tc)).get
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(
            performAction(stranger),
            routes.CheckClaimDetailsController.show
          )
        }

      "redirect to select duties when tax code is not in ndrc details" in {
        val correctedAmounts = Map(TaxCode("A00") -> Some(BigDecimal(100)), TaxCode("B00") -> None)
        val ndrcDetails      = List(
          NdrcDetails("A00", "200", "payment-method", "payment-reference", None)
        )

        val claim = claimGen.sample.getOrElse(fail("Failed to create claim"))

        val displayResponseDetail: DisplayResponseDetail =
          claim.answers.displayDeclaration.get.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
        val displayDeclaration                           =
          claim.answers.displayDeclaration.get.copy(displayResponseDetail = displayResponseDetail)

        val updatedClaim = OverpaymentsSingleClaim
          .unsafeModifyAnswers(
            claim,
            _.copy(displayDeclaration = Some(displayDeclaration), correctedAmounts = Some(correctedAmounts))
          )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(
          performAction(TaxCode("B00")),
          routes.SelectDutiesController.show
        )
      }
    }

    "Submit Enter Claim page" must {
      def performAction(taxCode: TaxCode, data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit(taxCode)(FakeRequest().withFormUrlEncodedBody(data*))

      "accept valid amount and redirect to the next claim" in
        forAll(claimGen) { claim =>
          val selected =
            claim.getSelectedDuties.get

          val selectedTaxCodesWithNext: Seq[(TaxCode, TaxCode)] =
            selected.dropRight(1).zip(selected.drop(1))

          selectedTaxCodesWithNext
            .foreach { case (taxCode, nextTaxCode) =>
              val paidAmount: BigDecimal =
                BigDecimal(claim.getNdrcDetailsFor(taxCode).get.amount)

              for (
                actualAmount <-
                  Seq(paidAmount - BigDecimal("0.01"), BigDecimal("0.01"), ZERO)
              ) {
                val claimAmount = paidAmount - actualAmount
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(claim))
                  mockStoreSession(
                    SessionData(
                      claim
                        .submitClaimAmount(taxCode, claimAmount)
                        .getOrFail
                    )
                  )(
                    Right(())
                  )
                }

                withClue(s"taxCode=$taxCode next=$nextTaxCode paid=$paidAmount claim=$claimAmount") {
                  checkIsRedirect(
                    performAction(taxCode, Seq("enter-claim-amount" -> formatAmount(claimAmount))),
                    routes.EnterClaimController.show(nextTaxCode)
                  )
                }
              }
            }
        }

      "redirect to check claim details when submitting a valid claim and not in change mode with complete reimbursements" in {
        val claim = completeClaimGen.sample.get.submitCheckYourAnswersChangeMode(false)

        val selected = claim.getSelectedDuties.get
        val taxCode  = selected.headOption.get

        val paidAmount: BigDecimal = BigDecimal(claim.getNdrcDetailsFor(taxCode).get.amount)

        val actualAmount = paidAmount - BigDecimal("0.01")
        val claimAmount  = paidAmount - actualAmount
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            SessionData(
              claim
                .submitClaimAmount(taxCode, claimAmount)
                .getOrFail
            )
          )(
            Right(())
          )
        }

        checkIsRedirect(
          performAction(taxCode, Seq("enter-claim-amount" -> formatAmount(claimAmount))),
          routes.CheckClaimDetailsController.show
        )
      }

      "redirect to check claim details when submitting a valid claim and this is the only remaining claim to enter and not in change mode" in {
        val correctedAmounts = Map(TaxCode("A00") -> Some(BigDecimal(100)), TaxCode("B00") -> None)
        val ndrcDetails      = List(
          NdrcDetails("A00", "200", "payment-method", "payment-reference", None),
          NdrcDetails("B00", "200", "payment-method", "payment-reference", None)
        )

        val claim = claimGen.sample.getOrElse(fail("Failed to create claim"))

        val displayResponseDetail: DisplayResponseDetail =
          claim.answers.displayDeclaration.get.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
        val displayDeclaration                           =
          claim.answers.displayDeclaration.get.copy(displayResponseDetail = displayResponseDetail)

        val updatedClaim = OverpaymentsSingleClaim
          .tryBuildFrom(
            claim.answers
              .copy(displayDeclaration = Some(displayDeclaration), correctedAmounts = Some(correctedAmounts))
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
          mockStoreSession(
            SessionData(
              updatedClaim
                .submitClaimAmount(TaxCode("B00"), 100)
                .getOrFail
            )
          )(
            Right(())
          )
        }

        checkIsRedirect(
          performAction(TaxCode("B00"), Seq("enter-claim-amount" -> formatAmount(100))),
          routes.CheckClaimDetailsController.show
        )
      }

      "redirect to check claim details when submitting a valid claim and this is the only remaining claim to enter and in change mode" in {
        val correctedAmounts = Map(TaxCode("A00") -> Some(BigDecimal(100)), TaxCode("B00") -> None)
        val ndrcDetails      = List(
          NdrcDetails("A00", "200", "payment-method", "payment-reference", None),
          NdrcDetails("B00", "200", "payment-method", "payment-reference", None)
        )

        val claim = claimGen.sample.getOrElse(fail("Failed to create claim"))

        val displayResponseDetail: DisplayResponseDetail =
          claim.answers.displayDeclaration.get.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
        val displayDeclaration                           =
          claim.answers.displayDeclaration.get.copy(displayResponseDetail = displayResponseDetail)

        val updatedClaim = OverpaymentsSingleClaim
          .tryBuildFrom(
            claim.answers
              .copy(displayDeclaration = Some(displayDeclaration), correctedAmounts = Some(correctedAmounts))
          )
          .getOrFail
          .withDutiesChangeMode(true)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
          mockStoreSession(
            SessionData(
              updatedClaim
                .submitClaimAmount(TaxCode("B00"), 100)
                .getOrFail
            )
          )(
            Right(())
          )
        }

        checkIsRedirect(
          performAction(TaxCode("B00"), Seq("enter-claim-amount" -> formatAmount(100))),
          routes.CheckClaimDetailsController.show
        )
      }

      "redirect to enter claim for the next tax code without a completed claim" in {
        val correctedAmounts =
          Map(TaxCode("A00") -> None, TaxCode("B00") -> Some(BigDecimal(100)), TaxCode("A20") -> None)
        val ndrcDetails      = List(
          NdrcDetails("A00", "200", "payment-method", "payment-reference", None),
          NdrcDetails("B00", "200", "payment-method", "payment-reference", None),
          NdrcDetails("A20", "200", "payment-method", "payment-reference", None)
        )

        val claim = claimGen.sample.getOrElse(fail("Failed to create claim"))

        val displayResponseDetail: DisplayResponseDetail =
          claim.answers.displayDeclaration.get.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
        val displayDeclaration                           =
          claim.answers.displayDeclaration.get.copy(displayResponseDetail = displayResponseDetail)

        val updatedClaim = OverpaymentsSingleClaim
          .tryBuildFrom(
            claim.answers
              .copy(displayDeclaration = Some(displayDeclaration), correctedAmounts = Some(correctedAmounts))
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
          mockStoreSession(
            SessionData(
              updatedClaim
                .submitClaimAmount(TaxCode("A00"), 100)
                .getOrFail
            )
          )(
            Right(())
          )
        }

        checkIsRedirect(
          performAction(TaxCode("A00"), Seq("enter-claim-amount" -> formatAmount(100))),
          routes.EnterClaimController.show(TaxCode("A20"))
        )
      }

      "reject invalid amount and display error" in
        forAll(claimGen) { claim =>
          claim.getSelectedDuties.get
            .foreach { taxCode =>
              val paidAmount: BigDecimal =
                BigDecimal(claim.getNdrcDetailsFor(taxCode).get.amount)

              for claimAmount <- Seq(ZERO, paidAmount + BigDecimal("0.01")) do {
                val actualAmount = paidAmount - claimAmount
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(claim))
                }

                withClue(s"taxCode=$taxCode paid=$paidAmount claim=$claimAmount") {
                  checkPageIsDisplayed(
                    performAction(taxCode, Seq("enter-claim-amount" -> formatAmount(claimAmount))),
                    if TaxCodes.custom.contains(taxCode) then
                      messageFromMessageKey(
                        "enter-claim.title",
                        taxCode.value,
                        messageFromMessageKey(s"select-duties.duty.$taxCode")
                      )
                    else
                      messageFromMessageKey(
                        "enter-claim.title.excise",
                        messages(s"duty-type.${taxCode.dutyType.repr}"),
                        taxCode.value,
                        messages(s"excise-category.${taxCode.exciseCategory.map(_.repr).getOrElse("none")}")
                      )
                    ,
                    doc => {
                      assertPageContent(doc, claim, taxCode, Some(actualAmount))
                      assertShowsInputError(doc, Some(m("enter-claim-amount.error.amount")))
                    },
                    expectedStatus = BAD_REQUEST
                  )
                }
              }
            }
        }

      "reject empty amount and display error" in
        forAll(claimGen) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          val taxCode: TaxCode =
            claim.getSelectedDuties.get.head

          checkPageIsDisplayed(
            performAction(taxCode, Seq("enter-claim-amount" -> "")),
            if TaxCodes.custom.contains(taxCode) then
              messageFromMessageKey(
                "enter-claim.title",
                taxCode.value,
                messageFromMessageKey(s"select-duties.duty.$taxCode")
              )
            else
              messageFromMessageKey(
                "enter-claim.title.excise",
                messages(s"duty-type.${taxCode.dutyType.repr}"),
                taxCode.value,
                messages(s"excise-category.${taxCode.exciseCategory.map(_.repr).getOrElse("none")}")
              )
            ,
            doc => {
              assertPageContent(doc, claim, taxCode, None)
              assertShowsInputError(doc, Some(m("enter-claim-amount.error.required")))
            },
            expectedStatus = BAD_REQUEST
          )
        }

      "redirect to select duties page when no duties selected" in {
        val claim = OverpaymentsSingleClaim
          .tryBuildFrom(
            claimGen.sample.getOrElse(fail("Failed to create claim")).answers.copy(correctedAmounts = None)
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(TaxCode("A00")),
          routes.SelectDutiesController.show
        )
      }

      "redirect to select duties page when tax code has not been selected" in {
        val correctedAmounts = Map(
          TaxCode("A00") -> None
        )

        val claim = completeClaimGen.sample.get.submitCheckYourAnswersChangeMode(false)

        val displayResponseDetail: DisplayResponseDetail =
          claim.answers.displayDeclaration.get.displayResponseDetail
        val displayDeclaration                           =
          claim.answers.displayDeclaration.get.copy(displayResponseDetail = displayResponseDetail)

        val updatedClaim = OverpaymentsSingleClaim.unsafeModifyAnswers(
          claim,
          _.copy(displayDeclaration = Some(displayDeclaration), correctedAmounts = Some(correctedAmounts))
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(performAction(TaxCode("B00")), routes.SelectDutiesController.show)
      }

      "redirect to enter mrn page when ndrc details are missing for tax code" in {
        val correctedAmounts = Map(
          TaxCode("A00") -> None,
          TaxCode("B00") -> Some(BigDecimal(200))
        )
        val ndrcDetails      = NdrcDetails("B00", "100", "payment-method", "payment-reference", None)

        val claim = completeClaimGen.sample.get.submitCheckYourAnswersChangeMode(false)

        val displayResponseDetail: DisplayResponseDetail =
          claim.answers.displayDeclaration.get.displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
        val displayDeclaration                           =
          claim.answers.displayDeclaration.get.copy(displayResponseDetail = displayResponseDetail)

        val updatedClaim = OverpaymentsSingleClaim.unsafeModifyAnswers(
          claim,
          _.copy(displayDeclaration = Some(displayDeclaration), correctedAmounts = Some(correctedAmounts))
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(performAction(TaxCode("A00")), routes.EnterMovementReferenceNumberController.show)
      }
    }
  }
}
