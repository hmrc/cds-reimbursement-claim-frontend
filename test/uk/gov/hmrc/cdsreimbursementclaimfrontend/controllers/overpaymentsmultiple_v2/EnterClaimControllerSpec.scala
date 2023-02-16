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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2

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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
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

  private val messagesKey: String = "multiple-enter-claim"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  def validateEnterClaimPage(
    doc: Document,
    pageIndex: Int,
    mrn: MRN,
    taxCode: TaxCode,
    actualAmountOpt: Option[BigDecimal]
  ) = {
    formAction(doc)               shouldBe routes.EnterClaimController.submit(pageIndex, taxCode).url
    assertPageElementsByIdAndExpectedText(doc)(
      "MRN"                             -> mrn.value,
      "multiple-enter-claim.inset-text" -> m("multiple-enter-claim.inset-text"),
      "multiple-enter-claim-label"      -> m("multiple-enter-claim.actual-amount"),
      "multiple-enter-claim-hint"       -> m(
        "multiple-enter-claim.actual-amount.hint",
        taxCode,
        m(s"select-duties.duty.$taxCode")
      )
    )
    assertPageInputsByIdAndExpectedValue(doc)(
      "multiple-enter-claim"            ->
        actualAmountOpt.fold("")(a => s"${a.toPoundSterlingString.drop(1)}")
    )
  }

  "EnterClaimController" when {

    "Show enter claim" must {

      def performAction(pageIndex: Int, taxCode: TaxCode): Future[Result] =
        controller.show(pageIndex, taxCode)(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction(1, TaxCode.A00)) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              journey
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

            selectedTaxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              val pageIndex = mrnIndex + 1

              checkPageIsDisplayed(
                performAction(pageIndex, taxCode),
                messageFromMessageKey(
                  s"$messagesKey.title",
                  taxCode,
                  messages(s"select-duties.duty.$taxCode"),
                  OrdinalNumeral(pageIndex)
                ),
                doc => validateEnterClaimPage(doc, pageIndex, mrn, taxCode, None)
              )
            }
          }
        }
      }

      "display the page back when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          journey.getMovementReferenceNumbers.get.zipWithIndex
            .foreach { case (mrn, mrnIndex) =>
              val selectedTaxCodes: Seq[TaxCode] =
                journey
                  .getSelectedDuties(mrn)
                  .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

              selectedTaxCodes.foreach { taxCode =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(SessionData(journey))
                }

                val pageIndex = mrnIndex + 1

                checkPageIsDisplayed(
                  performAction(pageIndex, taxCode),
                  messageFromMessageKey(
                    s"$messagesKey.title",
                    taxCode,
                    messages(s"select-duties.duty.$taxCode"),
                    OrdinalNumeral(pageIndex)
                  ),
                  doc =>
                    validateEnterClaimPage(doc, pageIndex, mrn, taxCode, journey.getCorrectedAmountFor(mrn, taxCode))
                )
              }
            }
        }
      }
    }

    "Submit Enter Claim page" must {
      def performAction(pageIndex: Int, taxCode: TaxCode, data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit(pageIndex, taxCode)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction(1, TaxCode.A00)) shouldBe NOT_FOUND
      }

      "accept valid amount and redirect to the next page" in
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes: Seq[TaxCode] =
              journey
                .getSelectedDuties(mrn)
                .getOrElse(fail("Expected non empty selection of duties, check journey generator."))

            selectedTaxCodes.zipWithIndex.foreach { case (taxCode, dutyIndex) =>
              val pageIndex    = mrnIndex + 1
              val actualAmount = BigDecimal("0.01")

              val expectedRoute =
                if (dutyIndex == selectedTaxCodes.size - 1) {
                  if (mrnIndex == mrns.size - 1)
                    routes.CheckClaimDetailsController.show
                  else
                    routes.SelectDutiesController.show(pageIndex + 1) // select duties for the next MRN
                } else
                  routes.EnterClaimController
                    .show(pageIndex, selectedTaxCodes(dutyIndex + 1)) // input amount for the next duty of current MRN

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
                mockStoreSession(
                  SessionData(
                    journey
                      .submitCorrectAmount(mrn, taxCode, actualAmount)
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
                  Seq("multiple-enter-claim" -> actualAmount.toPoundSterlingString.drop(1))
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
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                performAction(
                  pageIndex,
                  taxCode,
                  Seq("multiple-enter-claim" -> paidAmount.toPoundSterlingString.drop(1))
                ),
                messageFromMessageKey(
                  s"$messagesKey.title",
                  taxCode,
                  messages(s"select-duties.duty.$taxCode"),
                  OrdinalNumeral(pageIndex)
                ),
                doc => {
                  validateEnterClaimPage(doc, pageIndex, mrn, taxCode, Some(paidAmount))
                  assertShowsInputError(doc, Some(m("multiple-enter-claim.invalid.claim")))
                },
                expectedStatus = BAD_REQUEST
              )
            }
          }
        }
    }

  }

}
