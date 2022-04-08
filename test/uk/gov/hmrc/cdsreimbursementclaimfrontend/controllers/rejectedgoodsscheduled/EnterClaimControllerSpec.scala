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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterClaimControllerSpec.formatter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.SelectTaxCodesControllerSpec.genDutyWithRandomlySelectedTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementGen.genReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen.genTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import java.text.DecimalFormat

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
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsScheduledJourney = Some(RejectedGoodsScheduledJourney.empty(exampleEori))
  )

  "Enter Claim Controller" should {

    "not find the page if rejected goods feature is disabled" in forAll(genDutyWithRandomlySelectedTaxCode) {
      case (dutyType: DutyType, taxCode: TaxCode) =>
        featureSwitch.disable(Feature.RejectedGoods)

        status(controller.show(dutyType, taxCode)(FakeRequest())) shouldBe NOT_FOUND
    }

    "redirect to the select duty type page" when {

      "the user has not chosen duty type or tax code" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.showFirst()(FakeRequest()),
          routes.SelectDutyTypesController.show()
        )

      }

    }

    "show enter claim page" when {

      "the user has selected duty and tax codes for the first time" in forAll(genDutyWithRandomlySelectedTaxCode) {
        case (dutyType: DutyType, taxCode: TaxCode) =>
          val initialJourney = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(taxCode)))
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkPageIsDisplayed(
            controller.showFirst()(FakeRequest()),
            messageFromMessageKey(
              messageKey = s"$enterClaimKey.title",
              messages(s"duty-type.${dutyType.repr}"),
              taxCode.value
            )
          )

      }

      " the user revisits enter claim page again" in forAll(completeJourneyGen) { journey =>
        val dutyType: DutyType           = journey.getReimbursementClaims.head._1
        val taxCode: TaxCode             = journey.getReimbursementClaimsFor(dutyType).get.head._1
        val reimbursement: Reimbursement = journey.getReimbursementClaimsFor(dutyType).get.head._2.get
        val paidAmount: BigDecimal       = reimbursement.paidAmount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
        val shouldOfPaid: BigDecimal     = reimbursement.shouldOfPaid.setScale(2, BigDecimal.RoundingMode.HALF_UP)

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
            BigDecimal(elements.get(1).`val`()) should be(shouldOfPaid)
          }
        )
      }

    }

    "Submit enter claim page" must {

      "save user defined amounts and ask user to enter next amounts for upcoming reimbursement" in {
        forAll(Gen.oneOf(DutyTypes.custom), Gen.oneOf(DutyTypes.excise), genReimbursement) {
          (customDuty, exciseDuty, reimbursement) =>
            val initialJourney = RejectedGoodsScheduledJourney
              .empty(exampleEori)
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(customDuty, exciseDuty))
              .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(customDuty, Seq(customDuty.taxCodes(0))))
              .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exciseDuty, Seq(exciseDuty.taxCodes(1))))
              .getOrFail

            val updatedJourney = initialJourney
              .submitAmountForReimbursement(
                customDuty,
                customDuty.taxCodes(0),
                reimbursement.shouldOfPaid,
                reimbursement.paidAmount
              )
              .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
              mockStoreSession(SessionData(updatedJourney))(Right(()))
            }

            checkIsRedirect(
              controller.submit(customDuty, customDuty.taxCodes(0))(
                FakeRequest().withFormUrlEncodedBody(
                  Seq(
                    s"$enterClaimKey.paid-amount"   -> reimbursement.paidAmount.toString,
                    s"$enterClaimKey.actual-amount" -> reimbursement.shouldOfPaid.toString
                  ): _*
                )
              ),
              routes.EnterClaimController.show(exciseDuty, exciseDuty.taxCodes(1))
            )
        }
      }

      "save user defined amounts and redirect to the next page" in {
        forAll(Gen.oneOf(DutyTypes.custom), genReimbursement) { (dutyType, reimbursement) =>
          val initialJourney = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(dutyType.taxCodes(0))))
            .getOrFail

          val updatedJourney = initialJourney
            .submitAmountForReimbursement(
              dutyType,
              dutyType.taxCodes(0),
              reimbursement.shouldOfPaid,
              reimbursement.paidAmount
            )
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            controller.submit(dutyType, dutyType.taxCodes(0))(
              FakeRequest().withFormUrlEncodedBody(
                Seq(
                  s"$enterClaimKey.paid-amount"   -> formatter.format(reimbursement.paidAmount),
                  s"$enterClaimKey.actual-amount" -> formatter.format(reimbursement.shouldOfPaid)
                ): _*
              )
            ),
            "/rejected-goods/scheduled/check-claim" //FIXME: routes.CheckClaimController.show()
          )
        }
      }

      "show an error summary" when {
        "duty amounts are missing or invalid" in forAll(genDutyWithRandomlySelectedTaxCode) {
          case (dutyType: DutyType, taxCode: TaxCode) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkPageIsDisplayed(
              controller.submit(dutyType, taxCode)(
                FakeRequest().withFormUrlEncodedBody(
                  Seq(
                    s"$enterClaimKey.paid-amount"   -> "",
                    s"$enterClaimKey.actual-amount" -> "bad"
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
                  .text() shouldBe messageFromMessageKey(s"$enterClaimKey.paid-amount.error.required")
                doc
                  .select(".govuk-error-summary__list > li:nth-child(2) > a")
                  .text() shouldBe messageFromMessageKey(s"$enterClaimKey.actual-amount.error.invalid")
              },
              BAD_REQUEST
            )
        }

        "duty amounts are 0" in forAll(genDutyWithRandomlySelectedTaxCode) {
          case (dutyType: DutyType, taxCode: TaxCode) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
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
              doc => {
                doc
                  .select(".govuk-error-summary__list > li:nth-child(1) > a")
                  .text() shouldBe messageFromMessageKey(s"$enterClaimKey.paid-amount.error.zero")
                doc
                  .select(".govuk-error-summary__list > li:nth-child(2) > a")
                  .text() shouldBe messageFromMessageKey(s"$enterClaimKey.actual-amount.error.zero")
              },
              BAD_REQUEST
            )
        }

        "actual amount is greater or equal to paid amount" in {
          forAll(genDuty, genTaxCode, genReimbursement) { (duty, taxCode, reimbursement) =>
            val paidAmount   = reimbursement.paidAmount
            val actualAmount = reimbursement.shouldOfPaid + paidAmount

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkPageIsDisplayed(
              controller.submit(duty, taxCode)(
                FakeRequest().withFormUrlEncodedBody(
                  Seq(
                    s"$enterClaimKey.paid-amount"   -> paidAmount.toString,
                    s"$enterClaimKey.actual-amount" -> actualAmount.toString
                  ): _*
                )
              ),
              messageFromMessageKey(
                messageKey = s"$enterClaimKey.title",
                messages(s"duty-type.${duty.repr}"),
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

object EnterClaimControllerSpec {
  private val formatter = new DecimalFormat()

  formatter.setMinimumFractionDigits(2)
  formatter.setGroupingUsed(false)

  lazy val genDutyWithRandomlySelectedTaxCodeAndReimbursement: Gen[(DutyType, TaxCode, Reimbursement)] = for {
    duty          <- genDuty
    taxCode       <- Gen.oneOf(duty.taxCodes)
    reimbursement <- genReimbursement
  } yield (duty, taxCode, reimbursement)

}
