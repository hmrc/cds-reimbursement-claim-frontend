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
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterScheduledClaimController.enterScheduledClaimKey

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.SelectDutyCodesControllerSpec.genDutyWithRandomlySelectedTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterClaimController.findDutyTypeAndTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterClaimControllerSpec.genDutyWithRandomlySelectedTaxCodeAndReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterClaimControllerSpec.formatter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.taxCodesGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementGen.genReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import java.text.DecimalFormat
import scala.collection.SortedMap
import scala.collection.immutable.SortedMap

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

  "Select Duty Codes Controller" should {

    "not find the page if rejected goods feature is disabled" in forAll(genDutyWithRandomlySelectedTaxCode) {
      case (dutyType: DutyType, taxCode: TaxCode) =>
        featureSwitch.disable(Feature.RejectedGoods)

        status(controller.show(dutyType, taxCode)(FakeRequest())) shouldBe NOT_FOUND
    }

    "redirect to the enter claim page" when {

      "the user has not answered this question before" in forAll(genDutyWithRandomlySelectedTaxCode) {
        case (dutyType: DutyType, taxCode: TaxCode) =>
          val initialJourney = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(taxCode)))
          val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = initialJourney.toOption)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
          }

          checkIsRedirect(
            controller.iterate()(FakeRequest()),
            routes.EnterClaimController.show(dutyType, taxCode)
          )

      } // not answered question before

    } // redirect to  enter claim page

    "redirect to the check claim page" when {

      "the user has a complete reimbursement claim" in forAll(completeJourneyGen) { journey =>
        val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Option(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          "/scheduled/check-claim"
        )

      } // has complete claim

    } // redirect to check claim page

    "show enter claim page" when {

      "the user has selected duty and tax codes for the first time" in forAll(genDutyWithRandomlySelectedTaxCode) {
        case (dutyType: DutyType, taxCode: TaxCode) =>
          val initialJourney = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(taxCode)))
          val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = initialJourney.toOption)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
          }

          checkPageIsDisplayed(
            controller.show(dutyType, taxCode)(FakeRequest()),
            messageFromMessageKey(
              messageKey = s"$enterScheduledClaimKey.title",
              messages(s"duty-type.${dutyType.repr}"),
              taxCode.value
            )
          )

      } // user has selected tax and type

      " the user revisits enter claim page again" in forAll(completeJourneyGen) { journey =>
        val dutyType: DutyType           = journey.getReimbursementClaims.head._1
        val taxCode: TaxCode             = journey.getReimbursementClaimsFor(dutyType).get.head._1
        val reimbursement: Reimbursement = journey.getReimbursementClaimsFor(dutyType).get.head._2.get
        val paidAmount: BigDecimal       = reimbursement.paidAmount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
        val shouldOfPaid: BigDecimal     = reimbursement.shouldOfPaid.setScale(2, BigDecimal.RoundingMode.HALF_UP)

        val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Option(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          controller.show(dutyType, taxCode)(FakeRequest()),
          messageFromMessageKey(
            messageKey = s"$enterScheduledClaimKey.title",
            messages(s"duty-type.${dutyType.repr}"),
            taxCode.value
          ),
          doc => {
            val elements = doc.select("input")
            BigDecimal(elements.get(0).`val`()) should be(paidAmount)
            BigDecimal(elements.get(1).`val`()) should be(shouldOfPaid)
          }
        )
      } // revisits page

    } //show enter claim page

    "Submit enter claim page" must {

      "save user defined amounts and ask user to enter next amounts for upcoming reimbursement" in {
        forAll(Gen.oneOf(DutyTypes.custom), Gen.oneOf(DutyTypes.excise), genReimbursement) {
          (customDuty, exciseDuty, reimbursement) =>

            val initialJourney = RejectedGoodsScheduledJourney
              .empty(exampleEori)

            val updatedJourney = initialJourney.flatMap(journey => journey.submitAmountForReimbursement(customDuty, customDuty.taxCodes(0), reimbursement.paidAmount, reimbursement.shouldOfPaid))
            val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = updatedJourney.toOption)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
            }

            checkIsRedirect(
              controller.submit(exciseDuty, exciseDuty.taxCodes(0))(
                FakeRequest().withFormUrlEncodedBody(
                  Seq(
                    s"$enterScheduledClaimKey.paid-amount"   -> formatter.format(reimbursement.paidAmount),
                    s"$enterScheduledClaimKey.actual-amount" -> formatter.format(reimbursement.shouldOfPaid)
                  ): _*
                )
              ),
              routes.EnterClaimController.show(exciseDuty, exciseDuty.taxCodes(0))
            )
        }
      }

    } // submit end

  } //controller end
} //end

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
