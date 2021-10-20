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

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.whoIsMakingTheClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{DeclarantTypeAnswer, SelectNumberOfClaimsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{ContactName, Email}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{Eori, GGCredId, MRN}

import scala.concurrent.Future

class SelectWhoIsMakingTheClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectWhoIsMakingTheClaimController = instanceOf[SelectWhoIsMakingTheClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val testCases = Table(
    ("NumberOfClaimsType", "JourneyBindable"),
    (SelectNumberOfClaimsAnswer.Individual, JourneyBindable.Single),
    (SelectNumberOfClaimsAnswer.Multiple, JourneyBindable.Multiple),
    (SelectNumberOfClaimsAnswer.Scheduled, JourneyBindable.Scheduled)
  )

  private def sessionWithClaimState(
    declarantTypeAnswer: Option[DeclarantTypeAnswer],
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      = DraftClaim.blank.copy(
      declarantTypeAnswer = declarantTypeAnswer,
      selectNumberOfClaimsAnswer = numberOfClaims
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

  "Select who is making the claim controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(): Future[Result] = controller.selectDeclarantType(journeyBindable)(FakeRequest())
        val (session, _, _)                 = sessionWithClaimState(None, Some(numberOfClaims))

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

      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.selectDeclarantType(journeyBindable)(FakeRequest())

      "the user has not answered this question before" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val draftC285Claim                = sessionWithClaimState(None, Some(numberOfClaims))._3
          .copy(movementReferenceNumber = Some(sample[MRN]))
        val (session, fillingOutClaim, _) = sessionWithClaimState(None, Some(numberOfClaims))
        val updatedJourney                = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(journeyBindable),
          messageFromMessageKey(s"$whoIsMakingTheClaimKey.title")
        )

      }

      "the user has answered this question before" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val declarantType                 = DeclarantTypeAnswer.Importer
        val answers                       = declarantType
        val draftC285Claim                = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(movementReferenceNumber = Some(sample[MRN]))
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))
        val updatedJourney                = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(journeyBindable),
          messageFromMessageKey(s"$whoIsMakingTheClaimKey.title")
        )
      }
    }

    "handle submit requests" when {

      def performAction(data: Seq[(String, String)], journeyBindable: JourneyBindable): Future[Result] =
        controller.selectDeclarantTypeSubmit(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses a valid option" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val declarantType                 = DeclarantTypeAnswer.Importer
        val answers                       = declarantType
        val draftC285Claim                = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumber = Some(sample[MRN])
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))
        val updatedJourney                = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq(whoIsMakingTheClaimKey -> "0"), journeyBindable),
          routes.CheckContactDetailsMrnController.addDetailsShow(journeyBindable)
        )
      }

      "the user does not select an option" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val declarantType                 = DeclarantTypeAnswer.Importer
        val answers                       = declarantType
        val draftC285Claim                = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumber = Some(sample[MRN])
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))
        val updatedJourney                = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(Seq.empty, journeyBindable),
          messageFromMessageKey(s"$whoIsMakingTheClaimKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$whoIsMakingTheClaimKey.error.required"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val declarantType                 = DeclarantTypeAnswer.Importer
        val answers                       = declarantType
        val draftC285Claim                = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumber = Some(sample[MRN])
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))
        val updatedJourney                = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(Seq(whoIsMakingTheClaimKey -> "10"), journeyBindable),
          messageFromMessageKey(s"$whoIsMakingTheClaimKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$whoIsMakingTheClaimKey.invalid"
            ),
          BAD_REQUEST
        )
      }

      "the user amends their answer" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(data: Seq[(String, String)], journeyBindable: JourneyBindable): Future[Result] =
          controller.changeDeclarantTypeSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val declarantType = DeclarantTypeAnswer.Importer

        val answers                       = declarantType
        val draftC285Claim                = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            declarantTypeAnswer = Some(answers),
            movementReferenceNumber = Some(sample[MRN])
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))
        val updatedJourney                = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq(whoIsMakingTheClaimKey -> "0"), journeyBindable),
          routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
        )
      }

    }

  }

}
