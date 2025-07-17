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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.SelectDutiesControllerSpec.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory

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

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(OverpaymentsScheduledJourney.empty(exampleEori))

  "Select Tax Codes Controller" should {

    "show select customs tax codes page" when {

      "the user has not answered this question before" in forAll(genCustomsDuty) { (dutyType: DutyType) =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          controller.show(dutyType)(FakeRequest()),
          messageFromMessageKey(s"select-duty-codes.title.${dutyType.repr}"),
          doc => {
            selectedCheckBox(doc) shouldBe empty
            formAction(doc)       shouldBe routes.SelectDutiesController.submit(dutyType).url
          }
        )
      }

      "user has previously selected duty types" in forAll(completeJourneyGen, genCustomsDuty) {
        (journey, dutyType: DutyType) =>
          val updatedJourney = journey.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType)).getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedJourney))
          }

          checkPageIsDisplayed(
            controller.show(dutyType)(FakeRequest()),
            messageFromMessageKey(s"select-duty-codes.title.${dutyType.repr}")
          )
      }

    }

    "show select excise categories page" when {

      "the user has not answered this question before" in {
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          controller.show(DutyType.Excise)(FakeRequest()),
          messageFromMessageKey(
            s"select-excise-categories.title"
          ),
          doc => {
            selectedCheckBox(doc) shouldBe empty
            formAction(doc)       shouldBe routes.SelectDutiesController.submitExciseCategories.url
          }
        )
      }

      "user has previously selected excise categories" in forAll(
        completeJourneyGen,
        genExciseCategory
      ) { (journey, exciseCategory: ExciseCategory) =>
        val updatedJourney =
          journey
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
            .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)))
            .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourney))
        }

        checkPageIsDisplayed(
          controller.show(DutyType.Excise)(FakeRequest()),
          messageFromMessageKey(
            s"select-excise-categories.title"
          )
        )
      }

    }

    "tick existing customs tax codes" when {

      "select tax code page is shown" in forAll(genCustomsDutyWithRandomlySelectedTaxCode) {
        case (dutyType: DutyType, taxCode: TaxCode) =>
          val journey = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(dutyType, Seq(taxCode)))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            controller.show(dutyType)(FakeRequest()),
            messageFromMessageKey(s"select-duty-codes.title.${dutyType.repr}"),
            doc => isCheckboxChecked(doc, taxCode.value) shouldBe true
          )
      }

    }

    "tick existing excise categories" when {

      "select excise categories page is shown" in forAll(genExciseCategory) { (exciseCategory: ExciseCategory) =>
        val journey = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
          .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          controller.show(DutyType.Excise)(FakeRequest()),
          messageFromMessageKey(
            "select-excise-categories.title"
          ),
          doc => isCheckboxChecked(doc, exciseCategory.repr) shouldBe true
        )
      }

    }
  }

  "Submit Select Tax Codes page" must {

    "save user selected tax codes and redirect to the next page" when {

      "no other selected duties remaining" in forAll(genCustomsDutyWithRandomlySelectedTaxCode) {
        case (duty, taxCode) =>
          val initialJourney = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(duty))
            .getOrFail

          val updatedJourney =
            initialJourney.selectAndReplaceTaxCodeSetForDutyType(duty, Seq(taxCode)).getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            controller.submit(duty)(
              FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> taxCode.value)
            ),
            routes.EnterClaimController.show(duty, taxCode)
          )
      }
    }

    "save user selected tax codes and ask user to enter reclaim amount (even if there are other duty types)" in {

      forAll(Gen.oneOf(DutyTypes.custom), Gen.oneOf(DutyTypes.excise)) { (customDuty, exciseDuty) =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(customDuty, exciseDuty))
          .getOrFail

        val taxCode: TaxCode = customDuty.taxCodes.head
        val updatedJourney   =
          initialJourney.selectAndReplaceTaxCodeSetForDutyType(customDuty, Seq(taxCode)).getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          controller.submit(customDuty)(
            FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> customDuty.taxCodes.head.value)
          ),
          routes.EnterClaimController.show(customDuty, taxCode)
        )
      }
    }

    "show an error summary" when {

      "no tax code is selected" in forAll { (dutyType: DutyType) =>
        val initialJourney = journeyWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          controller.submit(dutyType)(FakeRequest().withFormUrlEncodedBody(s"select-duty-codes" -> "")),
          messageFromMessageKey(s"select-duty-codes.title.${dutyType.repr}"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-duty-codes.error.required"
            ),
          BAD_REQUEST
        )
      }
    }

  }
}

object SelectDutiesControllerSpec {

  lazy val genCustomsDutyWithRandomlySelectedTaxCode: Gen[(DutyType, TaxCode)] = for
    duty    <- genCustomsDuty
    taxCode <- Gen.oneOf(duty.taxCodes)
  yield (duty, taxCode)

  lazy val genExciseCategoryWithRandomlySelectedTaxCode: Gen[(ExciseCategory, TaxCode)] = for
    exciseCategory <- Gen.oneOf(ExciseCategory.all)
    taxCode        <- Gen.oneOf(exciseCategory.taxCodes)
  yield (exciseCategory, taxCode)

}
