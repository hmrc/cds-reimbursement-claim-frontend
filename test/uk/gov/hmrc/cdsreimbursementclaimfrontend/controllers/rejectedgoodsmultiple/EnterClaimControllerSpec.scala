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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

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

  def validateEnterClaimPage(
    doc: Document,
    pageIndex: Int,
    mrn: MRN,
    taxCode: TaxCode,
    actualAmountOpt: Option[BigDecimal]
  ) = {
    formAction(doc)                 shouldBe routes.EnterClaimController.submit(pageIndex, taxCode).url
    assertPageElementsByIdAndExpectedText(doc)(
      "MRN"                               -> mrn.value,
      "enter-claim-agent-fees-disclaimer" -> m("enter-claim.inset-text"),
      "enter-claim-amount-label"          -> m("enter-claim-amount.label")
    )
    assertPageInputsByIdAndExpectedValue(doc)(
      "enter-claim-amount"                ->
        actualAmountOpt.fold("")(a => formatAmount(a))
    )
  }

  "EnterClaimController" when {

    "Show enter claim" must {

      def performAction(pageIndex: Int, taxCode: TaxCode): Future[Result] =
        controller.show(pageIndex, taxCode)(FakeRequest())

      "display the page" in {
        forAll(incompleteClaimWithSelectedDutiesGen(5)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(claim))
              }

              val pageIndex = mrnIndex + 1

              checkPageIsDisplayed(
                performAction(pageIndex, taxCode),
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
                doc => validateEnterClaimPage(doc, pageIndex, mrn, taxCode, None)
              )
            }
          }
        }
      }

      "display the page in the change mode" in {
        forAll(incompleteClaimWithCompleteClaimsGen(5)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(claim))
              }

              val pageIndex = mrnIndex + 1

              checkPageIsDisplayed(
                performAction(pageIndex, taxCode),
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
                  val amount = for
                    amountPaid    <- claim.getAmountPaidFor(mrn, taxCode)
                    correctAmount <- claim.getCorrectedAmountFor(mrn, taxCode)
                  yield amountPaid - correctAmount
                  validateEnterClaimPage(doc, pageIndex, mrn, taxCode, amount)
                }
              )
            }
          }
        }
      }

      "display the page back when in the change mode from CYA" in {
        forAll(completeClaimGen) { claim =>
          claim.getMovementReferenceNumbers.get.zipWithIndex
            .foreach { case (mrn, mrnIndex) =>
              val selectedTaxCodes: Seq[TaxCode] =
                claim
                  .getSelectedDuties(mrn)
                  .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

              selectedTaxCodes.foreach { taxCode =>
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(claim))
                }

                val pageIndex = mrnIndex + 1

                checkPageIsDisplayed(
                  performAction(pageIndex, taxCode),
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
                    val amount = for
                      amountPaid    <- claim.getAmountPaidFor(mrn, taxCode)
                      correctAmount <- claim.getCorrectedAmountFor(mrn, taxCode)
                    yield amountPaid - correctAmount
                    validateEnterClaimPage(doc, pageIndex, mrn, taxCode, amount)
                  }
                )
              }
            }
        }
      }

      "redirect to select duties when no duties selected" in {
        forAll(incompleteClaimWithMrnsGen(5)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            val pageIndex = mrnIndex + 1

            redirectLocation(performAction(pageIndex, TaxCode.A00)) shouldBe Some(
              routes.SelectDutiesController.show(pageIndex).url
            )
          }
        }
      }

      "redirect to MRN not found when page index is out of bounds" in {
        forAll(incompleteClaimWithSelectedDutiesGen(5)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            checkPageIsDisplayed(
              performAction(100, selectedTaxCodes.head),
              "This Movement Reference Number (MRN) does not exist",
              expectedStatus = BAD_REQUEST
            )
          }
        }
      }
    }

    "Show first by index enter claim" must {

      def performAction(pageIndex: Int): Future[Result] =
        controller.showFirstByIndex(pageIndex)(FakeRequest())

      "redirect to enter claim" in {
        forAll(incompleteClaimWithSelectedDutiesGen(5)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(claim))
              }

              val pageIndex = mrnIndex + 1

              redirectLocation(performAction(pageIndex)) shouldBe Some(
                routes.EnterClaimController.show(pageIndex, selectedTaxCodes.head).url
              )
            }
          }
        }
      }

      "redirect to select duties when no duties selected" in {
        forAll(incompleteClaimWithMrnsGen(5)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            val pageIndex = mrnIndex + 1

            redirectLocation(performAction(pageIndex)) shouldBe Some(routes.SelectDutiesController.showFirst.url)
          }
        }
      }
    }

    "Submit Enter Claim page" must {
      def performAction(pageIndex: Int, taxCode: TaxCode, data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit(pageIndex, taxCode)(FakeRequest().withFormUrlEncodedBody(data*))

      "accept valid amount and redirect to the next page" in
        forAll(incompleteClaimWithSelectedDutiesGen(2)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            selectedTaxCodes.zipWithIndex.foreach { case (taxCode, dutyIndex) =>
              val pageIndex   = mrnIndex + 1
              val claimAmount = BigDecimal("0.01")

              val expectedRoute =
                if dutyIndex == selectedTaxCodes.size - 1 then {
                  if mrnIndex == mrns.size - 1 then routes.CheckClaimDetailsController.show
                  else routes.SelectDutiesController.show(pageIndex + 1) // select duties for the next MRN
                } else
                  routes.EnterClaimController
                    .show(pageIndex, selectedTaxCodes(dutyIndex + 1)) // input amount for the next duty of current MRN

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(claim))
                mockStoreSession(
                  SessionData(
                    claim
                      .submitClaimAmount(mrn, taxCode, claimAmount)
                      .getOrFail
                  )
                )(
                  Right(())
                )
              }

              checkIsRedirect(
                performAction(
                  pageIndex,
                  taxCode,
                  Seq("enter-claim-amount" -> formatAmount(claimAmount))
                ),
                expectedRoute
              )
            }
          }
        }

      "reject invalid amount and display error message" in
        forAll(incompleteClaimWithSelectedDutiesGen(2)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            selectedTaxCodes.foreach { taxCode =>
              val pageIndex  = mrnIndex + 1
              val paidAmount = claim.getAmountPaidFor(mrn, taxCode).get

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(claim))
              }

              checkPageIsDisplayed(
                performAction(
                  pageIndex,
                  taxCode,
                  Seq("enter-claim-amount" -> formatAmount(paidAmount + BigDecimal("0.01")))
                ),
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
                  validateEnterClaimPage(doc, pageIndex, mrn, taxCode, Some(paidAmount + BigDecimal("0.01")))
                  assertShowsInputError(doc, Some(m("enter-claim-amount.error.amount")))
                },
                expectedStatus = BAD_REQUEST
              )
            }
          }
        }

      "redirect to select duties when no duties selected" in {
        forAll(incompleteClaimWithMrnsGen(2)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            val pageIndex = mrnIndex + 1

            redirectLocation(performAction(pageIndex, TaxCode.A00)) shouldBe Some(
              routes.SelectDutiesController.show(pageIndex).url
            )
          }
        }
      }

      "redirect to MRN not found when page index is out of bounds" in {
        forAll(incompleteClaimWithSelectedDutiesGen(2)) { case (claim, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              claim
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            checkPageIsDisplayed(
              performAction(100, selectedTaxCodes.head),
              "This Movement Reference Number (MRN) does not exist",
              expectedStatus = BAD_REQUEST
            )
          }
        }
      }

      "redirect to claims summary when claims are complete and not in change mode" in {
        forAll(completeClaimGen) { claim =>
          claim.getMovementReferenceNumbers.get.zipWithIndex
            .foreach { case (mrn, mrnIndex) =>
              val selectedTaxCodes: Seq[TaxCode] =
                claim
                  .getSelectedDuties(mrn)
                  .getOrElse(fail("Expected non empty selection of duties, check claim generator."))

              val updatedClaim = claim.submitCheckYourAnswersChangeMode(false)

              selectedTaxCodes.foreach { taxCode =>

                val pageIndex = mrnIndex + 1

                val claimAmount = BigDecimal("0.01")

                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(updatedClaim))
                  mockStoreSession(
                    SessionData(
                      updatedClaim
                        .submitClaimAmount(mrn, taxCode, claimAmount)
                        .getOrFail
                    )
                  )(
                    Right(())
                  )
                }

                checkIsRedirect(
                  performAction(pageIndex, taxCode, Seq("enter-claim-amount" -> formatAmount(claimAmount))),
                  routes.CheckClaimDetailsController.show.url
                )
              }
            }
        }
      }
    }

    "decide next route" must {
      "redirect to select duties when no duties selected" in {
        val (claim, mrns) = incompleteClaimWithMrnsGen(2).sample.get
        mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
          val invalidTaxCodes = TaxCodes.all
          for (invalidTaxCode <- invalidTaxCodes)
            controller.decideNextRoute(
              claim,
              mrnIndex + 1,
              mrn,
              invalidTaxCode
            ) shouldBe routes.SelectDutiesController
              .show(mrnIndex + 1)
        }
      }

      "redirect to select duties when invalid tax code" in {
        val (claim, mrns) = incompleteClaimWithSelectedDutiesGen(2).sample.get
        mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
          val invalidTaxCodes: Set[TaxCode] = TaxCodes.all.toSet -- claim.getSelectedDuties(mrn).get.toSet
          for (invalidTaxCode <- invalidTaxCodes)
            controller.decideNextRoute(
              claim,
              mrnIndex + 1,
              mrn,
              invalidTaxCode
            ) shouldBe routes.SelectDutiesController
              .show(mrnIndex + 1)
        }
      }

      "redirect to enter claim when valid tax code" in {
        val (claim, mrns) = incompleteClaimWithSelectedDutiesGen(2).sample.get
        mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
          val validTaxCodes: Seq[TaxCode] = claim.getSelectedDuties(mrn).get
          for (n <- 0 to validTaxCodes.size - 2)
            controller.decideNextRoute(
              claim,
              mrnIndex + 1,
              mrn,
              validTaxCodes(n)
            ) shouldBe routes.EnterClaimController
              .show(mrnIndex + 1, validTaxCodes(n + 1))
        }
      }

      "redirect to select duties for the next MRN when no more duties for the current MRN" in {
        val (claim, mrns) = incompleteClaimWithSelectedDutiesGen(2).sample.get
        mrns.dropRight(1).zipWithIndex.foreach { case (mrn, mrnIndex) =>
          val lastTaxCode: TaxCode = claim.getSelectedDuties(mrn).get.last
          controller.decideNextRoute(
            claim,
            mrnIndex + 1,
            mrn,
            lastTaxCode
          ) shouldBe routes.SelectDutiesController
            .show(mrnIndex + 2)
        }
      }

      "redirect to claims summary when claims are complete and not in change mode" in {
        val (claim, mrns) = incompleteClaimWithSelectedDutiesGen(2).sample.get
        mrns.zipWithIndex.last match {
          case (mrn, mrnIndex) =>
            val lastTaxCode: TaxCode = claim.getSelectedDuties(mrn).get.last
            controller.decideNextRoute(
              claim,
              mrnIndex + 1,
              mrn,
              lastTaxCode
            ) shouldBe routes.CheckClaimDetailsController.show
        }
      }
    }
  }
}
