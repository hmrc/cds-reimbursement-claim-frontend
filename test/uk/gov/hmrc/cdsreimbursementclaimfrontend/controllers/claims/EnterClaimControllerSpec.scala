/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimsAnswer.{CompleteClaimsAnswer, IncompleteClaimsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{moneyGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DutiesSelectedAnswer, SessionData, SignedInUserDetails, _}

import java.util.UUID
import scala.concurrent.Future

class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {
  lazy val controller: EnterClaimController            = instanceOf[EnterClaimController]
  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeClaimsAnswer: Option[ClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        claimsAnswer = maybeClaimsAnswer,
        movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  "Enter Claims Controller" must {

    def performAction(id: UUID): Future[Result] = controller.enterClaim(id)(FakeRequest())

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        val id = UUID.randomUUID()

        val answers = IncompleteClaimsAnswer.empty

        val (session, _, _) = sessionWithClaimState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(id),
          baseRoutes.StartController.start()
        )

      }

    }

    "redirect to claim page" when {

      "there  are no claims" in {
        def performAction(): Future[Result] = controller.startClaim()(FakeRequest())

        val answers = IncompleteClaimsAnswer.empty

        val draftC285Claim = sessionWithClaimState(Some(answers))._3

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.IneligibleController.ineligible()
        )

      }

      "number of claims match duties selected" in {
        def performAction(): Future[Result] = controller.startClaim()(FakeRequest())

        val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(TaxCode.A00))

        val claim = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = "A00")

        val answers = IncompleteClaimsAnswer(List(claim))

        val draftC285Claim = sessionWithClaimState(Some(answers))._3

        val updatedClaim = draftC285Claim.copy(dutiesSelectedAnswer = Some(dutiesSelectedAnswer))

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = updatedClaim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterClaimController.checkClaim()
        )

      }

      "number of claims is more than duties selected" in {
        def performAction(): Future[Result] = controller.startClaim()(FakeRequest())

        val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(TaxCode.A00))

        val claim1 = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = "A00")

        val claim2 = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = "A00")

        val answers = IncompleteClaimsAnswer(List(claim1, claim2))

        val draftC285Claim = sessionWithClaimState(Some(answers))._3

        val updatedClaim = draftC285Claim.copy(dutiesSelectedAnswer = Some(dutiesSelectedAnswer))

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = updatedClaim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterClaimController.checkClaim()
        )

      }

    }

    "display the page" when {

      "the user has not answered this question before" in {

        val claim = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = "A00")

        val answers = IncompleteClaimsAnswer(List(claim))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(claim.id),
          messageFromMessageKey("enter-claim.title", "Customs Duty - Code A00")
        )
      }

      "the user has answered this question before" in {
        val claim = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = true, taxCode = "A00")

        val answers = IncompleteClaimsAnswer(List(claim))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(claim.id),
          messageFromMessageKey("enter-claim.title", "Customs Duty - Code A00")
        )
      }

    }

    "handle submit requests" when {

      "user enters a valid paid and claim amount" in {

        def performAction(id: UUID, data: Seq[(String, String)]): Future[Result] =
          controller.enterClaimSubmit(id)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val claim = sample[Claim]
          .copy(
            claimAmount = BigDecimal(5.00).setScale(2),
            paidAmount = BigDecimal(10.00).setScale(2),
            isFilled = true,
            taxCode = "A00"
          )

        val answers = IncompleteClaimsAnswer(List(claim))

        val (session, _, _) = sessionWithClaimState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(
            claim.id,
            Seq(
              "enter-claim.paid-amount"  -> "10.00",
              "enter-claim.claim-amount" -> "5.00"
            )
          ),
          routes.EnterClaimController.checkClaim()
        )
      }

      "the user has checked the claims" in {

        def performAction(): Future[Result] = controller.checkClaimSubmit()(FakeRequest())

        val claim = sample[Claim]
          .copy(
            claimAmount = BigDecimal(5.00).setScale(2),
            paidAmount = BigDecimal(10.00).setScale(2),
            isFilled = true,
            taxCode = "A00"
          )

        val answers = CompleteClaimsAnswer(List(claim))

        val (session, _, _) = sessionWithClaimState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.BankAccountController.enterBankAccountDetails()
        )
      }

    }

    "show an error summary" when {

      "an invalid option value is submitted" in {

        def performAction(id: UUID, data: Seq[(String, String)]): Future[Result] =
          controller.enterClaimSubmit(id)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val claim = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = "A00")

        val answers = IncompleteClaimsAnswer(List(claim))

        val draftC285Claim = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            claim.id,
            Seq(
              "enter-claim.paid-amount"  -> "sdfdf",
              "enter-claim.claim-amount" -> "dfsfs"
            )
          ),
          messageFromMessageKey("enter-claim.title", "Customs Duty - Code A00"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-claim.paid-amount.error.invalid"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-claim.claim-amount.error.invalid"
            )
          },
          BAD_REQUEST
        )
      }

    }

    "display the check your claim page" when {

      "all the claims have been entered" in {

        val claim = sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = true, taxCode = "A00")

        val answers = IncompleteClaimsAnswer(List(claim))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(claim.id),
          messageFromMessageKey("enter-claim.title", "Customs Duty - Code A00")
        )

      }

    }

  }

  "Entry Number Claim Amount Validation" must {
    val form        = EnterClaimController.entryClaimAmountForm
    val paidAmount  = "enter-claim.paid-amount"
    val claimAmount = "enter-claim.claim-amount"

    val goodData = Map(
      paidAmount  -> "99999999999.99",
      claimAmount -> "0.01"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "paidAmount" should {
      "Accept shortest possible paidAmount" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible paidAmount" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject paidAmount decimals only too many" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
      "Reject paidAmount too many decimals" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
      "Reject paidAmount too long" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
      "Reject negative numbers" in {
        val errors = form.bind(goodData.updated(paidAmount, "-1")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
    }

    "claimAmount" should {
      "Accept shortest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject claimAmount decimals only too many" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too many decimals" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too long" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject negative numbers" in {
        val errors = form.bind(goodData.updated(claimAmount, "-1")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject when claimAmount > paidAmount" in {
        val data   = Map(
          paidAmount  -> "100",
          claimAmount -> "101.00"
        )
        val errors = form.bind(data).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.claim")
      }
    }
  }

  "MRN Claim Amount Validation" must {
    val form        = EnterClaimController.mrnClaimAmountForm(BigDecimal("99999999999.99"))
    val claimAmount = "enter-claim"

    val goodData = Map(
      claimAmount -> "0.01"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "claimAmount" should {
      "Accept shortest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject claimAmount too many decimal digits" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too many decimals" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too long" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject negative numbers" in {
        val errors = form.bind(goodData.updated(claimAmount, "-1")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject when claimAmount > paidAmount" in {
        val testForm = EnterClaimController.mrnClaimAmountForm(BigDecimal("100.00"))
        val data     = Map(claimAmount -> "101.00")
        val errors   = testForm.bind(data).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.claim")
      }
    }
  }
}
