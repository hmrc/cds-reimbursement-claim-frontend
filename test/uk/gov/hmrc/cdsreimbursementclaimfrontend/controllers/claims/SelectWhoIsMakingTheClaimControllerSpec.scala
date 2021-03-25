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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.{CompleteDeclarantTypeAnswer, IncompleteDeclarantTypeAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId}

import scala.concurrent.Future

class SelectWhoIsMakingTheClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectWhoIsMakingTheClaimController = instanceOf[SelectWhoIsMakingTheClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    declarantTypeAnswer: Option[DeclarantTypeAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(declarantTypeAnswer = declarantTypeAnswer)
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

  "Select who is making the claim controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.selectDeclarantType()(FakeRequest())

        val answers = IncompleteDeclarantTypeAnswer.empty

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
        def performAction(): Future[Result] = controller.selectDeclarantType()(FakeRequest())

        val answers = IncompleteDeclarantTypeAnswer.empty

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
          performAction(),
          messageFromMessageKey("select-who-is-making-the-claim.title"),
          doc =>
            doc
              .select("a.govuk-back-link")
              .attr("href") shouldBe
              routes.EnterDeclarationDetailsController.enterDeclarationDetails().url
        )
      }

      "the user has answered this question before" in {
        def performAction(): Future[Result] = controller.selectDeclarantType()(FakeRequest())

        val declarantType = DeclarantType.Importer

        val answers = CompleteDeclarantTypeAnswer(declarantType)

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
          performAction(),
          messageFromMessageKey("select-who-is-making-the-claim.title"),
          doc =>
            doc
              .select("a.govuk-back-link")
              .attr("href") shouldBe routes.EnterDeclarationDetailsController.enterDeclarationDetails().url
        )
      }

      "the user has come from the CYA page and is amending their answer" in {

        def performAction(): Future[Result] = controller.changeDeclarantType()(FakeRequest())

        val declarantType = DeclarantType.Importer

        val answers: CompleteDeclarantTypeAnswer = CompleteDeclarantTypeAnswer(declarantType)

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-who-is-making-the-claim.title"),
          doc =>
            doc
              .select("a.govuk-back-link")
              .attr("href") shouldBe routes.CheckYourAnswersAndSubmitController.checkAllAnswers().url
        )

      }

    }

    "handle submit requests" when {

      "user chooses a valid option" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.selectDeclarantTypeSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val declarantType = DeclarantType.Importer

        val answers: CompleteDeclarantTypeAnswer = CompleteDeclarantTypeAnswer(declarantType)

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq("select-who-is-making-the-claim" -> "0")),
          routes.EnterClaimantDetailsAsIndividualController
            .enterClaimantDetailsAsIndividual()
        )
      }

      "the user amends their answer" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.changeDeclarantTypeSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val declarantType = DeclarantType.Importer

        val answers: CompleteDeclarantTypeAnswer = CompleteDeclarantTypeAnswer(declarantType)

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq("select-who-is-making-the-claim" -> "0")),
          routes.CheckYourAnswersAndSubmitController
            .checkAllAnswers()
        )
      }

    }

    "show an error summary" when {

      "the user does not select an option" in {
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.selectDeclarantTypeSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val declarantType = DeclarantType.Importer

        val answers: CompleteDeclarantTypeAnswer = CompleteDeclarantTypeAnswer(declarantType)

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
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
          messageFromMessageKey("select-who-is-making-the-claim.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"select-who-is-making-the-claim.error.required"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted to the server" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.selectDeclarantTypeSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val declarantType = DeclarantType.Importer

        val answers: CompleteDeclarantTypeAnswer = CompleteDeclarantTypeAnswer(declarantType)

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(Seq("select-who-is-making-the-claim" -> "10")),
          messageFromMessageKey("select-who-is-making-the-claim.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"select-who-is-making-the-claim.invalid"
            ),
          BAD_REQUEST
        )
      }

    }
  }

}
