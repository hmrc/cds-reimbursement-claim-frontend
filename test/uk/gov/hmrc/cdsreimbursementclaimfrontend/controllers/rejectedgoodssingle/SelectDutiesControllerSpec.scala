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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}

import scala.concurrent.Future

class SelectDutiesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with DeclarationSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

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
    claim: RejectedGoodsSingleClaim,
    selectedDuties: Seq[String]
  ): Unit = {
    selectedCheckBox(doc) should contain theSameElementsAs selectedDuties
    checkboxes(doc)       should containOnlyPairsOf(
      claim.getAvailableDuties.map { case (taxCode, _) =>
        (taxCode.value + " - " + messages(s"select-duties.duty.$taxCode"), taxCode.value)
      }
    )
  }

  "Select Tax Codes Controller" when {

    "Show select tax codes page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page the first time" in {
        val claim = RejectedGoodsSingleClaim
          .empty(exampleImportDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
          .getOrFail

        val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.single.title"),
          assertPageContent(_, claim, Seq.empty)
        )
      }

      "display the page when a tax code has already been selected before" in {
        forAll(completeClaimGen) { claim =>
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))

          val selectedDuties: Seq[String] =
            claim.getSelectedDuties.map(_.map(_.value)).getOrElse(Seq.empty)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
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

        val claim = RejectedGoodsSingleClaim
          .tryBuildFrom(
            RejectedGoodsSingleClaim.Answers(
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
  }

  "Submit Select Tax Codes page" must {
    def performAction(data: Seq[(String, String)] = Seq.empty): Future[Result] =
      controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

    "reject an empty tax code selection" in {

      val claim = RejectedGoodsSingleClaim
        .empty(exampleImportDeclaration.getDeclarantEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
        .getOrFail

      val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))

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

    "redirect to ineligible when no duties are available on submit" in {

      val displayResponseDetailWithoutDuties      =
        exampleImportDeclaration.displayResponseDetail.copy(ndrcDetails = None)
      val displayResponseDeclarationWithoutDuties =
        exampleImportDeclaration.copy(displayResponseDetailWithoutDuties)

      val claim = RejectedGoodsSingleClaim
        .tryBuildFrom(
          RejectedGoodsSingleClaim.Answers(
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

    "select valid tax codes when none have been selected before" in {
      forAll(importDeclarationGen) { importDeclaration =>
        val initialClaim = RejectedGoodsSingleClaim
          .empty(importDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail

        val availableTaxCodes = importDeclaration.getAvailableTaxCodes
        val selectedTaxCodes  =
          if availableTaxCodes.size > 1 then availableTaxCodes.drop(1)
          else availableTaxCodes

        val initialSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(initialClaim))

        val updatedClaim   = initialClaim.selectAndReplaceTaxCodeSetForReimbursement(selectedTaxCodes)
        val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = updatedClaim.toOption)

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

      val claim = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .getOrFail

      val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))

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
