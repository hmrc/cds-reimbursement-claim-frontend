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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.SelectDutiesControllerSpec.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.completeClaimGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.claimWithMrnAndDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators.EitherOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory.*

import scala.collection.immutable.SortedMap

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

  val session: SessionData = SessionData(RejectedGoodsScheduledClaim.empty(exampleEori))

  "Select Tax Codes Controller" should {

    "Show Select Tax Codes" should {

      "show select customs tax codes page" when {

        "the user has not answered this question before" in forAll(genCustomsDuty) { (dutyType: DutyType) =>
          val initialClaim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
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

        "user has previously selected duty types" in forAll(completeClaimGen, genCustomsDuty) {
          (claim, dutyType: DutyType) =>
            val updatedClaim = claim.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType)).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(updatedClaim))
            }

            checkPageIsDisplayed(
              controller.show(dutyType)(FakeRequest()),
              messageFromMessageKey(s"select-duty-codes.title.${dutyType.repr}")
            )
        }
      }

      "tick existing customs tax codes" when {

        "select tax code page is shown" in forAll(genCustomsDutyWithRandomlySelectedTaxCode) {
          case (dutyType: DutyType, taxCode: TaxCode) =>
            val claim = claimWithMrnAndDeclaration
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
              .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(dutyType, Seq(taxCode)))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            checkPageIsDisplayed(
              controller.show(dutyType)(FakeRequest()),
              messageFromMessageKey(s"select-duty-codes.title.${dutyType.repr}"),
              doc => isCheckboxChecked(doc, taxCode.value) shouldBe true
            )
        }
      }

      "redirect to select duty types" when {

        "no duty types are selected" in {
          val initialClaim = claimWithMrnAndDeclaration

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(controller.show(EuDuty)(FakeRequest()), routes.SelectDutyTypesController.show)
        }
      }
    }

    "Show Select Excise Categories" should {

      "show select excise categories page" when {

        "the user has not answered this question before" in {
          val initialClaim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
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
          completeClaimGen,
          genExciseCategory
        ) { (claim, exciseCategory: ExciseCategory) =>
          val updatedClaim =
            claim
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
              .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)))
              .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkPageIsDisplayed(
            controller.show(DutyType.Excise)(FakeRequest()),
            messageFromMessageKey(
              s"select-excise-categories.title"
            )
          )
        }
      }

      "tick existing excise categories" when {

        "select excise categories page is shown" in forAll(genExciseCategory) { (exciseCategory: ExciseCategory) =>
          val claim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
            .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
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

    "Show Select Excise Duties" should {

      "show select excise duty codes page" when {

        "the user has not answered this question before" in forAll(genExciseCategoryWithRandomlySelectedTaxCode) {
          (exciseCategory, _) =>
            val initialClaim = claimWithMrnAndDeclaration
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(Excise))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkPageIsDisplayed(
              controller.showExciseDuties(exciseCategory)(FakeRequest()),
              messageFromMessageKey(
                s"select-excise-duty-codes.title",
                messageFromMessageKey(s"select-excise-duty-codes.h1.${exciseCategory.repr}")
              ),
              doc => {
                selectedCheckBox(doc) shouldBe empty
                formAction(doc)       shouldBe routes.SelectDutiesController.submitExciseDuties(exciseCategory).url
              }
            )
        }

        "user has previously selected excise duty types" in forAll(genExciseCategoryWithRandomlySelectedTaxCode) {
          (exciseCategory, taxCode) =>
            val initialClaim = claimWithMrnAndDeclaration
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(Excise))
              .flatMap(_.selectAndReplaceTaxCodeSetForExciseCategory(exciseCategory, Seq(taxCode)))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkPageIsDisplayed(
              controller.showExciseDuties(exciseCategory)(FakeRequest()),
              messageFromMessageKey(
                s"select-excise-duty-codes.title",
                messageFromMessageKey(s"select-excise-duty-codes.h1.${exciseCategory.repr}")
              ),
              doc => isCheckboxChecked(doc, taxCode.value) shouldBe true
            )
        }
      }

      "redirect to select duty types" when {

        "no duty types are selected" in {
          val initialClaim = claimWithMrnAndDeclaration

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(controller.showExciseDuties(Beer)(FakeRequest()), routes.SelectDutyTypesController.show)
        }
      }
    }

    "Submit Select Tax Codes" should {

      "save user selected tax codes and redirect to the next page" when {

        "no other selected duties remaining" in forAll(genCustomsDutyWithRandomlySelectedTaxCode) {
          case (duty, taxCode) =>
            val initialClaim = claimWithMrnAndDeclaration
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(duty))
              .getOrFail

            val updatedClaim =
              initialClaim.selectAndReplaceTaxCodeSetForDutyType(duty, Seq(taxCode)).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(SessionData(updatedClaim))(Right(()))
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
          val initialClaim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(customDuty, exciseDuty))
            .getOrFail

          val taxCode: TaxCode = customDuty.taxCodes.head
          val updatedClaim     =
            initialClaim.selectAndReplaceTaxCodeSetForDutyType(customDuty, Seq(taxCode)).getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(updatedClaim))(Right(()))
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
          val initialClaim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
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

      "redirect to select duty type page" when {

        "no duty types are selected" in {
          val initialClaim = claimWithMrnAndDeclaration

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            controller.submit(DutyType.UkDuty)(FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> "A00")),
            routes.SelectDutyTypesController.show
          )
        }
      }

      "display select duties page and return 400 BAD_REQUEST" when {

        "duty submitted is of a duty type not selected" in {
          val initialClaim = RejectedGoodsScheduledClaim.unsafeModifyAnswers(
            claimWithMrnAndDeclaration,
            _.copy(correctedAmounts =
              Some(
                SortedMap(
                  DutyType.UkDuty -> SortedMap(
                    TaxCode.A00 -> Some(AmountPaidWithCorrect(BigDecimal("21.00"), BigDecimal("12.99")))
                  )
                )
              )
            )
          )

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageIsDisplayed(
            controller.submit(DutyType.EuDuty)(FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> "A50")),
            messageFromMessageKey(s"select-duty-codes.title.${DutyType.EuDuty.repr}"),
            expectedStatus = BAD_REQUEST
          )
        }
      }
    }

    "Submit Select Excise Categories" should {

      "save user selected excise categories and redirect to the next page" when {

        "no other selected duties remaining" in forAll(genExciseCategory) { (exciseCategory: ExciseCategory) =>
          val initialClaim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
            .getOrFail

          val updatedClaim =
            initialClaim.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)).getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(updatedClaim))(Right(()))
          }

          checkIsRedirect(
            controller.submitExciseCategories()(
              FakeRequest().withFormUrlEncodedBody(s"select-excise-categories[]" -> exciseCategory.repr)
            ),
            routes.SelectDutiesController.showExciseDuties(exciseCategory)
          )
        }
      }

      "save user selected excise categories and go to select excise duties page (even if there are other duty types)" in forAll(
        genExciseCategory,
        genCustomsDuty
      ) { (exciseCategory: ExciseCategory, customDutyType: DutyType) =>
        val initialClaim = claimWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise, customDutyType))
          .getOrFail

        val updatedClaim =
          initialClaim.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)).getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialClaim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          controller.submitExciseCategories()(
            FakeRequest().withFormUrlEncodedBody(s"select-excise-categories[]" -> exciseCategory.repr)
          ),
          routes.SelectDutiesController.showExciseDuties(exciseCategory)
        )
      }

      "show an error summary" when {

        "no excise category is selected" in forAll(genExciseCategory) { (exciseCategory: ExciseCategory) =>
          val initialClaim = claimWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.Excise))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageIsDisplayed(
            controller.submitExciseCategories()(FakeRequest().withFormUrlEncodedBody(s"select-excise-category" -> "")),
            messageFromMessageKey(s"select-excise-categories.title"),
            doc =>
              doc
                .select(".govuk-error-summary__list > li:nth-child(1) > a")
                .text() shouldBe messageFromMessageKey(
                s"select-excise-categories.error.required"
              ),
            BAD_REQUEST
          )
        }
      }

      "redirect to select duty type page" when {

        "no duty types are selected" in {
          val initialClaim = claimWithMrnAndDeclaration

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            controller.submitExciseCategories()(
              FakeRequest().withFormUrlEncodedBody(s"select-excise-categories[]" -> "beer")
            ),
            routes.SelectDutyTypesController.show
          )
        }
      }

      "display select duties page and return 400 BAD_REQUEST" when {

        "excise category submitted is of a duty type not selected" in {
          val initialClaim = RejectedGoodsScheduledClaim.unsafeModifyAnswers(
            claimWithMrnAndDeclaration,
            _.copy(correctedAmounts =
              Some(
                SortedMap(
                  DutyType.UkDuty -> SortedMap(
                    TaxCode.A00 -> Some(AmountPaidWithCorrect(BigDecimal("21.00"), BigDecimal("12.99")))
                  )
                )
              )
            )
          )

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageIsDisplayed(
            controller.submit(DutyType.EuDuty)(FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> "A50")),
            messageFromMessageKey(s"select-duty-codes.title.${DutyType.EuDuty.repr}"),
            expectedStatus = BAD_REQUEST
          )
        }
      }
    }

    "Submit Select Excise Duties" should {

      "save user selected excise tax codes and redirect to the next page" when {

        "no other selected duties remaining" in forAll(genExciseCategoryWithRandomlySelectedTaxCode) {
          case (exciseCategory, taxCode) =>
            val initialClaim = claimWithMrnAndDeclaration
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(Excise))
              .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)))
              .getOrFail

            val updatedClaim =
              initialClaim.selectAndReplaceTaxCodeSetForExciseCategory(exciseCategory, Seq(taxCode)).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(SessionData(updatedClaim))(Right(()))
            }

            checkIsRedirect(
              controller.submitExciseDuties(exciseCategory)(
                FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> taxCode.value)
              ),
              routes.EnterClaimController.show(DutyType.Excise, taxCode)
            )
        }
      }

      "save user selected tax codes and ask user to enter reclaim amount (even if there are other excise categories)" in {
        val exciseCategory = Wine
        val initialClaim   = claimWithMrnAndDeclaration
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(Excise))
          .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory, Spirits)))
          .getOrFail

        val taxCode: TaxCode = exciseCategory.taxCodes.headOption.get
        val updatedClaim     =
          initialClaim.selectAndReplaceTaxCodeSetForExciseCategory(exciseCategory, Seq(taxCode)).getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialClaim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          controller.submitExciseDuties(exciseCategory)(
            FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> taxCode.value)
          ),
          routes.EnterClaimController.show(Excise, taxCode)
        )
      }

      "show an error summary" when {

        "no tax code is selected" in forAll(genExciseCategoryWithRandomlySelectedTaxCode) {
          case (exciseCategory, taxCode) =>
            val initialClaim = claimWithMrnAndDeclaration
              .selectAndReplaceDutyTypeSetForReimbursement(Seq(Excise))
              .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(exciseCategory)))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkPageIsDisplayed(
              controller.submitExciseDuties(exciseCategory)(
                FakeRequest().withFormUrlEncodedBody(s"select-duty-codes" -> "")
              ),
              messageFromMessageKey(
                s"select-excise-duty-codes.title",
                messageFromMessageKey(s"select-excise-duty-codes.h1.${exciseCategory.repr}")
              ),
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

      "redirect to select duty type page" when {

        "no duty types are selected" in {
          val initialClaim = claimWithMrnAndDeclaration

          val exciseCategory = genExciseCategory.sample.get
          val taxCode        = Gen.oneOf(exciseCategory.taxCodes).sample.get

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            controller.submitExciseDuties(exciseCategory)(
              FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> taxCode.value)
            ),
            routes.SelectDutyTypesController.show
          )
        }
      }

      "display select duties page and return 400 BAD_REQUEST" when {

        "duty submitted is of a duty type not selected" in {
          val initialClaim = RejectedGoodsScheduledClaim.unsafeModifyAnswers(
            claimWithMrnAndDeclaration,
            _.copy(correctedAmounts =
              Some(
                SortedMap(
                  DutyType.UkDuty -> SortedMap(
                    TaxCode.A00 -> Some(AmountPaidWithCorrect(BigDecimal("21.00"), BigDecimal("12.99")))
                  )
                )
              )
            )
          )

          val exciseCategory = genExciseCategory.sample.get
          val taxCode        = Gen.oneOf(exciseCategory.taxCodes).sample.get

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageIsDisplayed(
            controller.submitExciseDuties(exciseCategory)(
              FakeRequest().withFormUrlEncodedBody(s"select-duty-codes[]" -> taxCode.value)
            ),
            messageFromMessageKey(
              s"select-excise-duty-codes.title",
              messageFromMessageKey(s"select-excise-duty-codes.h1.${exciseCategory.repr}")
            ),
            expectedStatus = BAD_REQUEST
          )
        }
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
