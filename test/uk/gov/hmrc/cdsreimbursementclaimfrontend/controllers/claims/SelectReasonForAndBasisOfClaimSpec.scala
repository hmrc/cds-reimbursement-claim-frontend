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

import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectReasonForBasisAndClaimController.SelectReasonForClaimAndBasis
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonAndBasisOfClaimAnswer.{CompleteReasonAndBasisOfClaimAnswer, IncompleteReasonAndBasisOfClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}

import scala.concurrent.Future

class SelectReasonForAndBasisOfClaimSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectReasonForBasisAndClaimController = instanceOf[SelectReasonForBasisAndClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeReasonAndBasisOfClaimAnswer: Option[ReasonAndBasisOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(reasonForBasisAndClaimAnswer = maybeReasonAndBasisOfClaimAnswer)
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

  "Select Reason for and Basis of claim controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.selectReasonForClaimAndBasis()(FakeRequest())

        val answers = IncompleteReasonAndBasisOfClaimAnswer.empty

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
        def performAction(): Future[Result] = controller.selectReasonForClaimAndBasis()(FakeRequest())

        val answers = IncompleteReasonAndBasisOfClaimAnswer.empty

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumber = sampleEntryNumberAnswer())
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-reason-and-basis-for-claim.title")
        )
      }

      "the user has answered this question before" in {
        def performAction(): Future[Result] = controller.selectReasonForClaimAndBasis()(FakeRequest())

        val mailForOrderGoods = ReasonForClaim.MailForOrderGoods
        val basisOfClaim      = BasisOfClaim.Miscellaneous

        val answers = CompleteReasonAndBasisOfClaimAnswer(SelectReasonForClaimAndBasis(basisOfClaim, mailForOrderGoods))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumber = sampleEntryNumberAnswer())
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-reason-and-basis-for-claim.title")
        )
      }
    }
    "handle submit requests" when {

      "user chooses a valid option" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.selectReasonForClaimAndBasisSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val mailForOrderGoods = ReasonForClaim.Overpayment
        val basisOfClaim      = BasisOfClaim.IncorrectCommodityCode

        val answers = CompleteReasonAndBasisOfClaimAnswer(SelectReasonForClaimAndBasis(basisOfClaim, mailForOrderGoods))

        val (session, _, _) = sessionWithClaimState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(
            Seq("select-reason-and-basis-for-claim.basis" -> "3", "select-reason-and-basis-for-claim.reason" -> "1")
          ),
          routes.EnterCommoditiesDetailsController.enterCommoditiesDetails(JourneyBindable.Single)
        )
      }

      "the user amends their answer" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.changeReasonForClaimAndBasisSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )
        val mailForOrderGoods                                          = ReasonForClaim.Overpayment
        val basisOfClaim                                               = BasisOfClaim.IncorrectCommodityCode

        val answers = CompleteReasonAndBasisOfClaimAnswer(SelectReasonForClaimAndBasis(basisOfClaim, mailForOrderGoods))

        val (session, _, _) = sessionWithClaimState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(
            Seq("select-reason-and-basis-for-claim.basis" -> "3", "select-reason-and-basis-for-claim.reason" -> "1")
          ),
          routes.CheckYourAnswersAndSubmitController.checkAllAnswers()
        )
      }

    }

    "show an error summary" when {

      "the user does not select anY options" in {
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.changeReasonForClaimAndBasisSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val mailForOrderGoods = ReasonForClaim.MailForOrderGoods
        val basisOfClaim      = BasisOfClaim.Miscellaneous

        val answers = CompleteReasonAndBasisOfClaimAnswer(SelectReasonForClaimAndBasis(basisOfClaim, mailForOrderGoods))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            reasonForBasisAndClaimAnswer = Some(answers),
            movementReferenceNumber = sampleEntryNumberAnswer()
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
          messageFromMessageKey("select-reason-and-basis-for-claim.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-reason-and-basis-for-claim.basis.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-reason-and-basis-for-claim.reason.error.required"
            )
          },
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.changeReasonForClaimAndBasisSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val mailForOrderGoods = ReasonForClaim.MailForOrderGoods
        val basisOfClaim      = BasisOfClaim.Miscellaneous

        val answers = CompleteReasonAndBasisOfClaimAnswer(SelectReasonForClaimAndBasis(basisOfClaim, mailForOrderGoods))

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(
            reasonForBasisAndClaimAnswer = Some(answers),
            movementReferenceNumber = sampleEntryNumberAnswer()
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq(
              "select-reason-and-basis-for-claim.basis"  -> "blah",
              "select-reason-and-basis-for-claim.reason" -> "blah"
            )
          ),
          messageFromMessageKey("select-reason-and-basis-for-claim.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-reason-and-basis-for-claim.basis.error.number"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-reason-and-basis-for-claim.reason.error.number"
            )
          },
          BAD_REQUEST
        )
      }

    }
  }

}
