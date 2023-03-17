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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import org.scalacheck.Gen
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
import play.api.test.Helpers.BAD_REQUEST
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
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
  val enterClaimKey: String            = "enter-scheduled-claim"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val journeyGen: Gen[OverpaymentsScheduledJourney] =
    buildJourneyGen(answersWithDutiesSelectedGen())

  "Enter Claim Controller" should {

    "not find the page if rejected goods feature is disabled" in forAll(journeyGen) { journey =>
      featureSwitch.disable(Feature.Overpayments_v2)

      val (dutyType, taxCode) = journey.getFirstDutyToClaim.get

      status(controller.show(dutyType, taxCode)(FakeRequest())) shouldBe NOT_FOUND
    }

    "redirect to the select duty type page" when {

      "the user has not chosen duty type or tax code" in {

        val journey = buildJourneyGen(answersUpToBasisForClaimGen()).sample.get
        val session = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.showFirst()(FakeRequest()),
          routes.SelectDutyTypesController.show
        )
      }
    }

    "show enter claim page" when {

      "the user has selected duty and tax codes for the first time" in forAll(journeyGen) { journey =>
        journey.getFirstDutyToClaim.map { case (dutyType: DutyType, taxCode: TaxCode) =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            controller.showFirst()(FakeRequest()),
            routes.EnterClaimController.show(dutyType, taxCode)
          )
        }

      }

      " the user revisits enter claim page again" in forAll(completeJourneyGen) { journey =>
        val dutyType: DutyType                   = journey.getReimbursementClaims.head._1
        val taxCode: TaxCode                     = journey.getReimbursementClaimsFor(dutyType).get.head._1
        val reimbursement: AmountPaidWithCorrect = journey.getReimbursementClaimsFor(dutyType).get.head._2.get
        val paidAmount: BigDecimal               = reimbursement.paidAmount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
        val correctAmount: BigDecimal            = reimbursement.correctAmount.setScale(2, BigDecimal.RoundingMode.HALF_UP)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          controller.show(dutyType, taxCode)(FakeRequest()),
          messageFromMessageKey(
            messageKey = s"$enterClaimKey.title",
            messages(s"duty-type.${dutyType.repr}"),
            taxCode.value
          ),
          doc => {
            val elements = doc.select("input")
            BigDecimal(elements.get(0).`val`()) should be(paidAmount)
            BigDecimal(elements.get(1).`val`()) should be(correctAmount)
          }
        )
      }

    }

    "Submit enter claim page" must {

      def performAction(dutyType: DutyType, taxCode: TaxCode, data: Seq[(String, String)]): Future[Result] =
        controller.submit(dutyType, taxCode)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "save user defined amounts and ask user to enter next amounts for upcoming reimbursement" in {
        forAll(journeyGen, amountPaidWithCorrectGen) {
          case (initialJourney, AmountPaidWithCorrect(paidAmount, correctAmount)) =>
            initialJourney.getSelectedDuties.foreach { case (dutyType, taxCodes) =>
              taxCodes.foreach { taxCode =>
                val updatedJourney = initialJourney
                  .submitCorrectAmount(
                    dutyType,
                    taxCode,
                    paidAmount,
                    correctAmount
                  )
                  .getOrFail

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(SessionData(initialJourney))
                  mockStoreSession(SessionData(updatedJourney))(Right(()))
                }

                val expectedRoute: Call =
                  updatedJourney.findNextSelectedTaxCodeAfter(dutyType, taxCode) match {
                    case Some((nextDutyType, nextTaxCode)) =>
                      routes.EnterClaimController.show(nextDutyType, nextTaxCode)
                    case None                              =>
                      routes.CheckClaimDetailsController.show
                  }

                checkIsRedirect(
                  performAction(
                    dutyType,
                    taxCode,
                    Seq(
                      "enter-scheduled-claim.paid-amount"   -> paidAmount.toString,
                      "enter-scheduled-claim.actual-amount" -> correctAmount.toString
                    )
                  ),
                  expectedRoute
                )
              }
            }
        }
      }

      "save user defined amounts and redirect to the check claim details page" in {
        forAll(completeJourneyGen, amountPaidWithCorrectGen) {
          case (initialJourney, AmountPaidWithCorrect(paidAmount, correctAmount)) =>
            initialJourney.getSelectedDuties.foreach { case (dutyType, taxCodes) =>
              taxCodes.foreach { taxCode =>
                val updatedJourney = initialJourney
                  .submitCorrectAmount(
                    dutyType,
                    taxCode,
                    paidAmount,
                    correctAmount
                  )
                  .getOrFail

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(SessionData(initialJourney))
                  mockStoreSession(SessionData(updatedJourney))(Right(()))
                }

                val expectedRoute: Call =
                  routes.CheckClaimDetailsController.show

                checkIsRedirect(
                  performAction(
                    dutyType,
                    taxCode,
                    Seq(
                      "enter-scheduled-claim.paid-amount"   -> paidAmount.toString,
                      "enter-scheduled-claim.actual-amount" -> correctAmount.toString
                    )
                  ),
                  expectedRoute
                )
              }
            }
        }
      }

      "show an error summary" when {
        "duty amounts are missing or invalid" in forAll(journeyGen) { journey =>
          journey.getSelectedDuties.foreach { case (dutyType, taxCodes) =>
            taxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                controller.submit(dutyType, taxCode)(
                  FakeRequest().withFormUrlEncodedBody(
                    Seq(
                      "enter-scheduled-claim.paid-amount"   -> "",
                      "enter-scheduled-claim.actual-amount" -> "bad"
                    ): _*
                  )
                ),
                messageFromMessageKey(
                  messageKey = s"$enterClaimKey.title",
                  messages(s"duty-type.${dutyType.repr}"),
                  taxCode.value
                ),
                doc => {
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(1) > a")
                    .text() shouldBe messageFromMessageKey("enter-scheduled-claim.paid-amount.error.required")
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(2) > a")
                    .text() shouldBe messageFromMessageKey("enter-scheduled-claim.actual-amount.error.invalid")
                },
                BAD_REQUEST
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                controller.submit(dutyType, taxCode)(
                  FakeRequest().withFormUrlEncodedBody(
                    Seq(
                      s"$enterClaimKey.paid-amount"   -> "0.00",
                      s"$enterClaimKey.actual-amount" -> "0.00"
                    ): _*
                  )
                ),
                messageFromMessageKey(
                  messageKey = s"$enterClaimKey.title",
                  messages(s"duty-type.${dutyType.repr}"),
                  taxCode.value
                ),
                doc =>
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(1) > a")
                    .text() shouldBe messageFromMessageKey(s"$enterClaimKey.paid-amount.error.zero"),
                BAD_REQUEST
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                controller.submit(dutyType, taxCode)(
                  FakeRequest().withFormUrlEncodedBody(
                    Seq(
                      s"$enterClaimKey.paid-amount"   -> "12.34",
                      s"$enterClaimKey.actual-amount" -> "12.34"
                    ): _*
                  )
                ),
                messageFromMessageKey(
                  messageKey = s"$enterClaimKey.title",
                  messages(s"duty-type.${dutyType.repr}"),
                  taxCode.value
                ),
                doc =>
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(1) > a")
                    .text() shouldBe messageFromMessageKey(s"$enterClaimKey.actual-amount.invalid.claim"),
                BAD_REQUEST
              )
            }
          }
        }
      }

    }
  }
}
