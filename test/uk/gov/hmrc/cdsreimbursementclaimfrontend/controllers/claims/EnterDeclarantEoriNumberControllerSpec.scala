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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{Eori, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{ContactName, Email}
import scala.concurrent.Future

class EnterDeclarantEoriNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val testCases = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  lazy val controller: EnterDeclarantEoriNumberController = instanceOf[EnterDeclarantEoriNumberController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeDeclarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      = DraftClaim.blank.copy(
      declarantEoriNumberAnswer = maybeDeclarantEoriNumberAnswer,
      movementReferenceNumber = Some(sample[MRN])
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

      "there is no journey status in the session" in forAll(testCases) { journeyBindable =>
        def performAction(): Future[Result] = controller.enterDeclarantEoriNumber(journeyBindable)(FakeRequest())

        val (session, _, _) = sessionWithClaimState(None)

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

      "the user has not answered this question before" in forAll(testCases) { journeyBindable =>
        def performAction(): Future[Result] = controller.enterDeclarantEoriNumber(journeyBindable)(FakeRequest())

        val draftC285Claim = sessionWithClaimState(None)._3

        val (session, fillingOutClaim, _) = sessionWithClaimState(None)

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

      "the user has answered this question before" in forAll(testCases) { journeyBindable =>
        def performAction(): Future[Result] = controller.enterDeclarantEoriNumber(journeyBindable)(FakeRequest())

        val answers = models.answers.DeclarantEoriNumberAnswer(Eori("GB03152858027018"))

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

      "user chooses enters a valid eori but there is no match" in forAll(testCases) { journeyBindable =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarantEoriNumberSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = models.answers.DeclarantEoriNumberAnswer(Eori("GB03152858027018"))

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

      "the user does not select an option" in forAll(testCases) { journeyBindable =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarantEoriNumberSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = models.answers.DeclarantEoriNumberAnswer(Eori("GB03152858027018"))

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

      "an invalid option value is submitted" in forAll(testCases) { journeyBindable =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarantEoriNumberSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = DeclarantEoriNumberAnswer(Eori("GB03152858027018"))

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
}
