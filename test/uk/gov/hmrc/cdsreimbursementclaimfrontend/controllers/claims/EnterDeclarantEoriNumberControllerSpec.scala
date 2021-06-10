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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarantEoriNumberController.DeclarantEoriNumber

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantEoriNumberAnswer.{CompleteDeclarantEoriNumberAnswer, IncompleteDeclarantEoriNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{numStringGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.{eoriGen, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, _}

import scala.concurrent.Future

class EnterDeclarantEoriNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: EnterDeclarantEoriNumberController = instanceOf[EnterDeclarantEoriNumberController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeDeclarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      declarantEoriNumberAnswer = maybeDeclarantEoriNumberAnswer,
      movementReferenceNumber = Some(MovementReferenceNumber(Right(MRN("mrn"))))
    )
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails =
      SignedInUserDetails(Some(email), eori, Email("email@email.com"), ContactName("Fred Bread"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  "Enter Declarant Eori Number Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.enterDeclarantEoriNumber()(FakeRequest())

        val answers = IncompleteDeclarantEoriNumberAnswer.empty

        val (session, _, _) = sessionWithClaimState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )

      }

    }

    "display the page" when {

      "the user has not answered this question before" in {
        def performAction(): Future[Result] = controller.enterDeclarantEoriNumber()(FakeRequest())

        val answers = IncompleteDeclarantEoriNumberAnswer.empty

        val draftC285Claim = sessionWithClaimState(Some(answers))._3

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title")
        )
      }

      "the user has answered this question before" in {
        def performAction(): Future[Result] = controller.enterDeclarantEoriNumber()(FakeRequest())

        val answers = CompleteDeclarantEoriNumberAnswer(DeclarantEoriNumber(Eori("GB03152858027018")))

        val draftC285Claim = sessionWithClaimState(Some(answers))._3

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title")
        )
      }

    }

    "handle submit requests" when {

      "user chooses enters a valid eori but there is no match" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarantEoriNumberSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = CompleteDeclarantEoriNumberAnswer(DeclarantEoriNumber(Eori("GB03152858027018")))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantEoriNumberAnswer = Some(answers)
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq("enter-declarant-eori-number" -> "GB03152858027018")),
          baseRoutes.IneligibleController.ineligible()
        )
      }

    }

    "show an error summary" when {

      "the user does not select an option" in {
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarantEoriNumberSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = CompleteDeclarantEoriNumberAnswer(DeclarantEoriNumber(Eori("GB03152858027018")))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantEoriNumberAnswer = Some(answers)
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq.empty
          ),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declarant-eori-number.error.required"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarantEoriNumberSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = CompleteDeclarantEoriNumberAnswer(DeclarantEoriNumber(Eori("GB03152858027018")))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantEoriNumberAnswer = Some(answers)
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq.empty
          ),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declarant-eori-number.error.required"
            ),
          BAD_REQUEST
        )
      }

    }
  }

  "Entry Claim Amount Validation" must {
    val form       = EnterDeclarantEoriNumberController.eoriNumberForm
    val eoriNumber = "enter-declarant-eori-number"

    val goodData = Map(
      eoriNumber -> sample[Eori].value
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "Eori" should {
      "Accept shortedt possible Eori" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(12))).errors
        errors shouldBe Nil
      }
      "Accept longest possible Eori" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(15))).errors
        errors shouldBe Nil
      }
      "Reject too short Eoris" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(11))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }
      "Reject too long Eoris" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(16))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }
      "Reject way too long Eoris" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(17))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

  }

}
