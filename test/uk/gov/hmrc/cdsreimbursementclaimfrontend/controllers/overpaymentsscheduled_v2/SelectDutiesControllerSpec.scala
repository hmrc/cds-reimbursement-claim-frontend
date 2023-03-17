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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2.SelectDutiesControllerSpec.genDutyWithRandomlySelectedTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.EitherOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.journeyWithMrnAndDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

class SelectDutiesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutiesController = instanceOf[SelectDutiesController]
  val selectDutyCodesKey: String         = "select-duty-codes"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val session: SessionData = SessionData.empty.copy(
    overpaymentsScheduledJourney = Some(OverpaymentsScheduledJourney.empty(exampleEori))
  )

  "Select Tax Codes Controller" should {

    "not find the page if overpayments feature is disabled" in forAll { dutyType: DutyType =>
      featureSwitch.disable(Feature.Overpayments_v2)

      status(controller.show(dutyType)(FakeRequest())) shouldBe NOT_FOUND
    }

    "show select tax codes page" when {

      "the user has not answered this question before" in forAll { dutyType: DutyType =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
          .getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          controller.show(dutyType)(FakeRequest()),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${dutyType.repr}")
          ),
          doc => {
            selectedCheckBox(doc) shouldBe empty
            formAction(doc)       shouldBe routes.SelectDutiesController.submit(dutyType).url
          }
        )
      }

      "user has previously selected duty types" in forAll(completeJourneyGen, genDuty) {
        (journey, dutyType: DutyType) =>
          val updatedJourney = journey.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType)).getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(updatedJourney))
          }

          checkPageIsDisplayed(
            controller.show(dutyType)(FakeRequest()),
            messageFromMessageKey(
              s"$selectDutyCodesKey.title",
              messageFromMessageKey(s"$selectDutyCodesKey.h1.${dutyType.repr}")
            )
          )
      }

    }

    "tick existing tax codes" when {

      "select tax code page is shown" in forAll(genDutyWithRandomlySelectedTaxCode) {
        case (dutyType: DutyType, taxCode: TaxCode) =>
          val journey = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(taxCode)))
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            controller.show(dutyType)(FakeRequest()),
            messageFromMessageKey(
              s"$selectDutyCodesKey.title",
              messageFromMessageKey(s"$selectDutyCodesKey.h1.${dutyType.repr}")
            ),
            doc => isCheckboxChecked(doc, taxCode.value) shouldBe true
          )
      }

    }
  }

  "Submit Select Tax Codes page" must {

    "not find the page if overpayments feature is disabled" in forAll { duty: DutyType =>
      featureSwitch.disable(Feature.Overpayments_v2)

      status(controller.submit(duty)(FakeRequest())) shouldBe NOT_FOUND
    }

    "save user selected tax codes and redirect to the next page" when {

      "no other selected duties remaining" in forAll(genDutyWithRandomlySelectedTaxCode) { case (duty, taxCode) =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(duty))
          .getOrFail

        val updatedJourney =
          initialJourney.selectAndReplaceTaxCodeSetForReimbursement(duty, Seq(taxCode)).getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          controller.submit(duty)(
            FakeRequest().withFormUrlEncodedBody(s"$selectDutyCodesKey[]" -> taxCode.value)
          ),
          routes.EnterClaimController.showFirst
        )
      }
    }

    "save user selected tax codes and ask user to select tax codes for the next available duty" in {

      forAll(Gen.oneOf(DutyTypes.custom), Gen.oneOf(DutyTypes.excise)) { (customDuty, exciseDuty) =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(customDuty, exciseDuty))
          .getOrFail

        val taxCode: TaxCode = customDuty.taxCodes.head
        val updatedJourney   =
          initialJourney.selectAndReplaceTaxCodeSetForReimbursement(customDuty, Seq(taxCode)).getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          controller.submit(customDuty)(
            FakeRequest().withFormUrlEncodedBody(s"$selectDutyCodesKey[]" -> customDuty.taxCodes(0).value)
          ),
          routes.SelectDutiesController.show(exciseDuty)
        )
      }
    }

    "show an error summary" when {

      "no tax code is selected" in forAll { dutyType: DutyType =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
          .getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          controller.submit(dutyType)(FakeRequest().withFormUrlEncodedBody(s"$selectDutyCodesKey" -> "")),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${dutyType.repr}")
          ),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"$selectDutyCodesKey.error.required"
            ),
          BAD_REQUEST
        )
      }
    }

  }
}

object SelectDutiesControllerSpec {

  lazy val genDutyWithRandomlySelectedTaxCode: Gen[(DutyType, TaxCode)] = for {
    duty    <- genDuty
    taxCode <- Gen.oneOf(duty.taxCodes)
  } yield (duty, taxCode)

}
