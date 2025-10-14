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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes

import scala.concurrent.Future

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

  def getHintText(document: Document, hintTextId: String) = {
    val hintTextElement = document.select(s"div#$hintTextId")

    if hintTextElement.hasText then Some(hintTextElement.html()) else None
  }

  private val messagesKey: String = "select-duties"

  def assertPageContent(
    doc: Document,
    claim: OverpaymentsSingleClaim,
    selectedDuties: Seq[String]
  ): Unit = {
    selectedCheckBox(doc) should contain theSameElementsAs selectedDuties
    checkboxes(doc)       should containOnlyPairsOf(
      claim.getAvailableDuties.map { case (taxCode, _) =>
        (taxCode.value + " - " + messages(s"select-duties.duty.$taxCode"), taxCode.value)
      }
    )
  }

  val claimGen: Gen[OverpaymentsSingleClaim] =
    buildClaimFromAnswersGen(answersUpToBasisForClaimGen(forcedTaxCodes = Seq(TaxCode.NI411)))
      .flatMap(j =>
        Gen
          .oneOf(j.getAvailableClaimTypes)
          .map(b => j.submitBasisOfClaim(b))
      )

  val claimWithNIExciseCodesGen: Gen[OverpaymentsSingleClaim] =
    buildClaimFromAnswersGen(
      answersUpToBasisForClaimGen(taxCodes = TaxCodes.excise, forcedTaxCodes = Seq(TaxCode.A00, TaxCode.B05))
    )
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectExciseValue))

  "Select Duties Controller" when {

    "Show select duties page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page the first time" in forAll(claimGen) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.single.title"),
          assertPageContent(_, claim, Seq.empty)
        )
      }

      "display the page the first time when NI excise code in ACC14 and IncorrectExciseValue" in forAll(
        claimWithNIExciseCodesGen
      ) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.single.title"),
          assertPageContent(_, claim, Seq.empty)
        )
      }

      "display the page when a tax code has already been selected before" in {
        forAll(completeClaimGen) { claim =>
          val selectedDuties: Seq[String] =
            claim.getSelectedDuties.map(_.map(_.value)).getOrElse(Seq.empty)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.single.title"),
            assertPageContent(_, claim, selectedDuties)
          )
        }
      }

      "redirect to ineligible when no duties are available" in {
        val displayResponseDetailWithoutDuties      =
          exampleImportDeclaration.displayResponseDetail.copy(ndrcDetails = None)
        val displayResponseDeclarationWithoutDuties = exampleImportDeclaration.copy(displayResponseDetailWithoutDuties)

        val claim = OverpaymentsSingleClaim
          .tryBuildFrom(
            OverpaymentsSingleClaim.Answers(
              userEoriNumber = exampleEori,
              movementReferenceNumber = Some(exampleMrn),
              importDeclaration = Some(
                displayResponseDeclarationWithoutDuties
                  .withDeclarationId(exampleMrn.value)
                  .withDeclarantEori(exampleEori)
              )
            )
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.IneligibleController.ineligible.url
        )
      }
    }

    "Submit Select Duties page" must {
      def performAction(data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty tax code selection" in {

        val claim = OverpaymentsSingleClaim
          .empty(exampleImportDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
          .getOrFail

        val updatedSession = SessionData.empty.copy(overpaymentsSingleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(Seq("select-duties" -> "")),
          messageFromMessageKey(s"$messagesKey.single.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "select valid tax codes when none have been selected before" in {
        forAll(importDeclarationGen) { importDeclaration =>
          val initialClaim = OverpaymentsSingleClaim
            .empty(importDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
            .getOrFail

          val availableTaxCodes = importDeclaration.getAvailableTaxCodes
          val selectedTaxCodes  =
            if availableTaxCodes.size > 1 then availableTaxCodes.drop(1)
            else availableTaxCodes

          val initialSession = SessionData.empty.copy(overpaymentsSingleClaim = Some(initialClaim))

          val updatedClaim   = initialClaim.selectAndReplaceTaxCodeSetForReimbursement(selectedTaxCodes)
          val updatedSession = SessionData.empty.copy(overpaymentsSingleClaim = updatedClaim.toOption)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(initialSession)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(selectedTaxCodes.map(taxCode => "select-duties[]" -> taxCode.value)),
            routes.EnterClaimController.showFirst
          )
        }
      }

      "redirect to ineligible when no duties are available on submit" in {
        val displayResponseDetailWithoutDuties      =
          exampleImportDeclaration.displayResponseDetail.copy(ndrcDetails = None)
        val displayResponseDeclarationWithoutDuties =
          exampleImportDeclaration.copy(displayResponseDetailWithoutDuties)
        val claim                                   = OverpaymentsSingleClaim
          .tryBuildFrom(
            OverpaymentsSingleClaim.Answers(
              userEoriNumber = exampleEori,
              movementReferenceNumber = Some(exampleMrn),
              importDeclaration = Some(
                displayResponseDeclarationWithoutDuties
                  .withDeclarationId(exampleMrn.value)
                  .withDeclarantEori(exampleEori)
              )
            )
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.IneligibleController.ineligible.url
        )
      }
    }

    "have CMA Eligible flag/Duties hint text" should {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "Acc14 excise code where the CMA eligible flag is true" in {

        val importDeclaration = buildImportDeclaration(
          dutyDetails = Seq(
            (TaxCode.A80, BigDecimal("200.00"), true),
            (TaxCode.A95, BigDecimal("171.05"), false)
          )
        )

        val claim = OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail

        val updatedSession = SessionData.empty.copy(overpaymentsSingleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        val hintText =
          Some("This duty is not eligible for Current Month Adjustment (CMA) repayment.")

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.single.title"),
          doc => {
            getHintText(doc, "select-duties-item-hint")   shouldBe None
            getHintText(doc, "select-duties-2-item-hint") shouldBe hintText
          }
        )
      }
    }
  }
}
