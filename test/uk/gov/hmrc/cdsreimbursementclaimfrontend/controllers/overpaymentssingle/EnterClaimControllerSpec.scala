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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
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
    featureSwitch.enable(Feature.Overpayments_v2)

  def assertPageContent(
    doc: Document,
    journey: OverpaymentsSingleJourney,
    taxCode: TaxCode,
    actualAmountOpt: Option[BigDecimal]
  ): Unit = {
    val paidAmount: BigDecimal =
      journey
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
             messages(s"duty-type.${DutyTypes.dutyTypeOf(taxCode).repr}"),
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

  val journeyGen: Gen[OverpaymentsSingleJourney] =
    buildJourneyFromAnswersGen(answersWithDutiesSelectedGen())

  "Enter Claim Controller" when {

    "Show first enter claim page" must {

      def performAction(): Future[Result] =
        controller.showFirst()(FakeRequest())

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in forAll(journeyGen) { journey =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        val expectedTaxCode: TaxCode =
          journey.getSelectedDuties.get.head

        checkIsRedirect(
          performAction(),
          routes.EnterClaimController.show(expectedTaxCode)
        )
      }

      "display the page back in the change mode" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          val expectedTaxCode: TaxCode =
            journey.getSelectedDuties.get.head

          checkIsRedirect(
            performAction(),
            routes.EnterClaimController.show(expectedTaxCode)
          )
        }

      "redirect to select duties page if no claims selected" in
        forAll(buildJourneyFromAnswersGen(answersUpToBasisForClaimGen())) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.show
          )
        }

    }

    "Show enter claim page by tax code" must {

      def performAction(taxCode: TaxCode): Future[Result] =
        controller.show(taxCode)(FakeRequest())

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction(TaxCode.A00)) shouldBe NOT_FOUND
      }

      "display the page" in forAll(journeyGen) { journey =>
        journey.getSelectedDuties.get.foreach { taxCode =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
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
                messages(s"duty-type.${TaxCodes.categoryOf(taxCode)}"),
                messages(s"duty-type.${DutyTypes.dutyTypeOf(taxCode).repr}"),
                taxCode.value
              )
            ,
            assertPageContent(_, journey, taxCode, None)
          )
        }
      }

      "display the page back in the change mode" in
        forAll(completeJourneyGen) { journey =>
          journey.getSelectedDuties.get.foreach { taxCode =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            val actualAmountOpt: Option[BigDecimal] =
              journey.answers.correctedAmounts.flatMap(_.get(taxCode).flatten).map(_.getAmount)

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
                  messages(s"duty-type.${TaxCodes.categoryOf(taxCode)}"),
                  messages(s"duty-type.${DutyTypes.dutyTypeOf(taxCode).repr}"),
                  taxCode.value
                )
              ,
              assertPageContent(_, journey, taxCode, actualAmountOpt)
            )
          }
        }

      "when wrong code, redirect to select duties page" in
        forAll(journeyGen) { journey =>
          val selected = journey.getSelectedDuties.get
          val stranger = TaxCodes.all.find(tc => !selected.contains(tc)).get
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(stranger),
            routes.SelectDutiesController.show
          )
        }

      "when wrong code, redirect to check claims if all amounts provided already" in
        forAll(buildJourneyFromAnswersGen(answersWithAllAmountsProvidedGen())) { journey =>
          val selected = journey.getSelectedDuties.get
          val stranger = TaxCodes.all.find(tc => !selected.contains(tc)).get
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(stranger),
            routes.CheckClaimDetailsController.show
          )
        }

    }

    "Submit Enter Claim page" must {
      def performAction(taxCode: TaxCode, data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit(taxCode)(FakeRequest().withFormUrlEncodedBody(data*))

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction(TaxCode.A00)) shouldBe NOT_FOUND
      }

      "accept valid amount and redirect to the next claim" in
        forAll(journeyGen) { journey =>
          val selected =
            journey.getSelectedDuties.get

          val selectedTaxCodesWithNext: Seq[(TaxCode, TaxCode)] =
            selected.dropRight(1).zip(selected.drop(1))

          selectedTaxCodesWithNext
            .foreach { case (taxCode, nextTaxCode) =>
              val paidAmount: BigDecimal =
                BigDecimal(journey.getNdrcDetailsFor(taxCode).get.amount)

              for (
                actualAmount <-
                  Seq(paidAmount - BigDecimal("0.01"), BigDecimal("0.01"), ZERO)
              ) {
                val claimAmount = paidAmount - actualAmount
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(journey))
                  mockStoreSession(
                    SessionData(
                      journey
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

      "reject invalid amount and display error" in
        forAll(journeyGen) { journey =>
          journey.getSelectedDuties.get
            .foreach { taxCode =>
              val paidAmount: BigDecimal =
                BigDecimal(journey.getNdrcDetailsFor(taxCode).get.amount)

              for claimAmount <- Seq(ZERO, paidAmount + BigDecimal("0.01")) do {
                val actualAmount = paidAmount - claimAmount
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(journey))
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
                        messages(s"duty-type.${TaxCodes.categoryOf(taxCode)}"),
                        messages(s"duty-type.${DutyTypes.dutyTypeOf(taxCode).repr}"),
                        taxCode.value
                      )
                    ,
                    doc => {
                      assertPageContent(doc, journey, taxCode, Some(actualAmount))
                      assertShowsInputError(doc, Some(m("enter-claim-amount.error.amount")))
                    },
                    expectedStatus = BAD_REQUEST
                  )
                }
              }
            }
        }

      "reject empty amount and display error" in
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          val taxCode: TaxCode =
            journey.getSelectedDuties.get.head

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
                messages(s"duty-type.${TaxCodes.categoryOf(taxCode)}"),
                messages(s"duty-type.${DutyTypes.dutyTypeOf(taxCode).repr}"),
                taxCode.value
              )
            ,
            doc => {
              assertPageContent(doc, journey, taxCode, None)
              assertShowsInputError(doc, Some(m("enter-claim-amount.error.required")))
            },
            expectedStatus = BAD_REQUEST
          )
        }
    }
  }
}
