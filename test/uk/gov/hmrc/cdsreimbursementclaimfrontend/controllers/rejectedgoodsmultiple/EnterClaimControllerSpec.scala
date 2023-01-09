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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen
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

  private val messagesKey: String = EnterClaimController.key

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def validateEnterClaimPage(
    doc: Document,
    pageIndex: Int,
    amountPaid: String,
    claimAmount: String,
    taxCode: TaxCode
  ) = {
    hasContinueButton(doc)
    doc.select("#amount-paid").text() shouldBe amountPaid
    doc.select("input").attr("value") shouldBe claimAmount
    formAction(
      doc
    )                                 shouldBe s"/claim-back-import-duty-vat/rejected-goods/multiple/enter-claim/$pageIndex/$taxCode"
  }

  "EnterClaimController" when {

    "Show enter claim amount" must {

      def performAction(pageIndex: Int, taxCode: TaxCode): Future[Result] =
        controller.show(pageIndex, taxCode)(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        forAll(Gen.choose(1, 1000), TaxCodeGen.genTaxCode) { (pageIndex: Int, taxCode: TaxCode) =>
          status(performAction(pageIndex, taxCode)) shouldBe NOT_FOUND
        }
      }

      "display the page if mrn exists and tax code has been selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(9)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes = journey.getSelectedDuties(mrn).get
            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              val amountPaid = journey.getAmountPaidFor(mrn, taxCode).get

              checkPageIsDisplayed(
                performAction(mrnIndex + 1, taxCode),
                messageFromMessageKey(
                  s"$messagesKey.multiple.title",
                  taxCode.value,
                  messages(s"select-duties.duty.$taxCode"),
                  OrdinalNumber.label(mrnIndex + 1)
                ),
                doc => validateEnterClaimPage(doc, mrnIndex + 1, amountPaid.toPoundSterlingString, "", taxCode)
              )
            }
          }
        }
      }

      "re-display the page with existing claim amount" in {
        forAll(completeJourneyGen) { case journey =>
          val mrns = journey.answers.movementReferenceNumbers.get
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes = journey.getSelectedDuties(mrn).get
            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              val amountPaid  = journey.getAmountPaidFor(mrn, taxCode).get
              val claimAmount = journey.getReimbursementClaimFor(mrn, taxCode).get

              val claimAmountString = formatAmount(claimAmount)

              checkPageIsDisplayed(
                performAction(mrnIndex + 1, taxCode),
                messageFromMessageKey(
                  s"$messagesKey.multiple.title",
                  taxCode.value,
                  messages(s"select-duties.duty.$taxCode"),
                  OrdinalNumber.label(mrnIndex + 1)
                ),
                doc =>
                  validateEnterClaimPage(
                    doc,
                    mrnIndex + 1,
                    amountPaid.toPoundSterlingString,
                    claimAmountString,
                    taxCode
                  )
              )
            }
          }
        }
      }
    }

    "Submit claim amount" must {

      def performAction(pageIndex: Int, taxCode: TaxCode, claimAmount: String): Future[Result] =
        controller.submit(pageIndex, taxCode)(
          FakeRequest()
            .withFormUrlEncodedBody("enter-claim.rejected-goods.claim-amount" -> claimAmount.toString())
        )

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        forAll(Gen.choose(1, 1000), TaxCodeGen.genTaxCode) { (pageIndex: Int, taxCode: TaxCode) =>
          status(performAction(pageIndex, taxCode, formatAmount(BigDecimal("1.00")))) shouldBe NOT_FOUND
        }
      }

      "save the amount and redirect to the next page" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(9)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes = journey.getSelectedDuties(mrn).get

            selectedTaxCodes.foreach { taxCode =>
              val amountPaid  = journey.getAmountPaidFor(mrn, taxCode).get
              val claimAmount = BigDecimal(formatAmount(amountPaid / 2))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
                mockStoreSession(
                  SessionData(journey.submitAmountForReimbursement(mrn, taxCode, claimAmount).getOrFail)
                )(
                  Right(())
                )
              }

              checkIsRedirect(
                performAction(mrnIndex + 1, taxCode, claimAmount.toString()),
                if (taxCode === selectedTaxCodes.last) {
                  if (mrnIndex === mrns.size - 1)
                    s"/claim-back-import-duty-vat/rejected-goods/multiple/check-claim"
                  else
                    s"/claim-back-import-duty-vat/rejected-goods/multiple/select-duties/${mrnIndex + 2}"
                } else
                  s"/claim-back-import-duty-vat/rejected-goods/multiple/enter-claim/${mrnIndex + 1}/${nextTaxCode(selectedTaxCodes, taxCode)}"
              )
            }
          }
        }
      }

      "re-display the form if claim amount is invalid" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(9)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes = journey.getSelectedDuties(mrn).get
            selectedTaxCodes.foreach { taxCode =>
              val amountPaid  = journey.getAmountPaidFor(mrn, taxCode).get
              val claimAmount = BigDecimal(formatAmount(amountPaid + 0.01))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                performAction(mrnIndex + 1, taxCode, claimAmount.toString()),
                messageFromMessageKey(
                  s"$messagesKey.multiple.title",
                  taxCode.value,
                  messages(s"select-duties.duty.$taxCode"),
                  OrdinalNumber.label(mrnIndex + 1)
                ),
                doc =>
                  validateEnterClaimPage(
                    doc,
                    mrnIndex + 1,
                    amountPaid.toPoundSterlingString,
                    claimAmount.toString(),
                    taxCode
                  ),
                expectedStatus = 400
              )
            }
          }
        }
      }

    }
  }

}
