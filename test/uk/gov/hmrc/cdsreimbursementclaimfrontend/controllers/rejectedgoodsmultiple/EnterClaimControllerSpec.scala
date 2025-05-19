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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

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

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

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

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        forAll(Gen.choose(1, 1000), TaxCodeGen.genTaxCode) { (pageIndex: Int, taxCode: TaxCode) =>
          status(performAction(pageIndex, taxCode)) shouldBe NOT_FOUND
        }
      }

      "display the page" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              journey
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
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
        forAll(incompleteJourneyWithCompleteClaimsGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              journey
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
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
                    amountPaid    <- journey.getAmountPaidFor(mrn, taxCode)
                    correctAmount <- journey.getCorrectedAmountFor(mrn, taxCode)
                  yield amountPaid - correctAmount
                  validateEnterClaimPage(doc, pageIndex, mrn, taxCode, amount)
                }
              )
            }
          }
        }
      }

      "display the page back when in the change mode from CYA" in {
        forAll(completeJourneyGen) { journey =>
          journey.getMovementReferenceNumbers.get.zipWithIndex
            .foreach { case (mrn, mrnIndex) =>
              val selectedTaxCodes: Seq[TaxCode] =
                journey
                  .getSelectedDuties(mrn)
                  .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

              selectedTaxCodes.foreach { taxCode =>
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(journey))
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
                      amountPaid    <- journey.getAmountPaidFor(mrn, taxCode)
                      correctAmount <- journey.getCorrectedAmountFor(mrn, taxCode)
                    yield amountPaid - correctAmount
                    validateEnterClaimPage(doc, pageIndex, mrn, taxCode, amount)
                  }
                )
              }
            }
        }
      }
    }

    "Submit Enter Claim page" must {
      def performAction(pageIndex: Int, taxCode: TaxCode, data: Seq[(String, String)]): Future[Result] =
        controller.submit(pageIndex, taxCode)(FakeRequest().withFormUrlEncodedBody(data*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
      }

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        forAll(Gen.choose(1, 1000), TaxCodeGen.genTaxCode) { (pageIndex: Int, taxCode: TaxCode) =>
          status(
            performAction(pageIndex, taxCode, Seq("enter-claim-amount" -> formatAmount(BigDecimal("1.00"))))
          ) shouldBe NOT_FOUND
        }
      }

      "accept valid amount and redirect to the next page" in
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              journey
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

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
                mockGetSession(SessionData(journey))
                mockStoreSession(
                  SessionData(
                    journey
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
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              journey
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

            selectedTaxCodes.foreach { taxCode =>
              val pageIndex  = mrnIndex + 1
              val paidAmount = journey.getAmountPaidFor(mrn, taxCode).get

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
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
    }
  }
}
