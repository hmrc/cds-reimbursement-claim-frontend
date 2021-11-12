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

import cats.implicits.catsSyntaxOptionId
import org.scalatest.OptionValues
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectBasisForClaimController.selectBasisForClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{BasisOfClaimAnswer, BasisOfClaims, TypeOfClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class SelectBasisForClaimControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with OptionValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectBasisForClaimController = instanceOf[SelectBasisForClaimController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  featureSwitch.NorthernIreland.disable()

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeReasonForClaim: Option[BasisOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        basisOfClaimAnswer = maybeReasonForClaim,
        movementReferenceNumber = Some(sample[MRN])
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

  "Select basis of claim controller" must {

    def performAction(): Future[Result] = controller.selectBasisForClaim(JourneyBindable.Single)(FakeRequest())

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {
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

      "the user has not answered this question before and the NI feature switch is enabled" in {
        def performAction(): Future[Result] = controller.selectBasisForClaim(JourneyBindable.Single)(FakeRequest())

        featureSwitch.NorthernIreland.enable()

        val draftC285Claim                = sessionWithClaimState(None)._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$selectBasisForClaimKey.title")
        )
      }

      "the user has not answered this question before and the NI feature switch is disabled" in {
        def performAction(): Future[Result] = controller.selectBasisForClaim(JourneyBindable.Single)(FakeRequest())

        featureSwitch.NorthernIreland.disable()

        val draftC285Claim                = sessionWithClaimState(None)._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$selectBasisForClaimKey.title")
        )
      }

      "the user has answered this question before and the NI feature switch is enabled" in {
        def performAction(): Future[Result] = controller.selectBasisForClaim(JourneyBindable.Single)(FakeRequest())

        featureSwitch.NorthernIreland.enable()
        val basisOfClaimAnswer = BasisOfClaimAnswer.EndUseRelief.some

        val draftC285Claim                = sessionWithClaimState(basisOfClaimAnswer)._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(basisOfClaimAnswer)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$selectBasisForClaimKey.title")
        )
      }

      "the user has answered this question before and the NI feature switch is disabled" in {
        def performAction(): Future[Result] = controller.selectBasisForClaim(JourneyBindable.Single)(FakeRequest())

        featureSwitch.NorthernIreland.disable()

        val basisOfClaimAnswer = BasisOfClaimAnswer.EndUseRelief

        val draftC285Claim                = sessionWithClaimState(basisOfClaimAnswer.some)._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(basisOfClaimAnswer.some)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$selectBasisForClaimKey.title")
        )
      }

      "the user has come from the CYA page and is amending their answer" in {

        val basisOfClaimAnswer = BasisOfClaimAnswer.EndUseRelief.some

        val draftC285Claim                = sessionWithClaimState(basisOfClaimAnswer)._3
          .copy(
            basisOfClaimAnswer = basisOfClaimAnswer
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(basisOfClaimAnswer)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$selectBasisForClaimKey.title")
        )

      }

    }

    "handle submit requests" when {

      "user chooses a valid option" in {
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.selectBasisForClaimSubmit(JourneyBindable.Single)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val basisOfClaimAnswer = BasisOfClaimAnswer.EndUseRelief.some

        val draftC285Claim = sessionWithClaimState(basisOfClaimAnswer)._3.copy(basisOfClaimAnswer = basisOfClaimAnswer)

        val (session, fillingOutClaim, _) = sessionWithClaimState(basisOfClaimAnswer)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq(selectBasisForClaimKey -> "2")),
          routes.EnterCommoditiesDetailsController.enterCommoditiesDetails(JourneyBindable.Single)
        )
      }

      "the user amends their answer" in {
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.selectBasisForClaimSubmit(JourneyBindable.Single)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val claim = sample(genValidDraftClaim(TypeOfClaimAnswer.Individual))
        val idx   = BasisOfClaims.indexOf(claim.basisOfClaimAnswer.value)

        val session = SessionData.empty.copy(
          journeyStatus = FillingOutClaim(
            sample[GGCredId],
            sample[SignedInUserDetails],
            claim
          ).some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(Seq(selectBasisForClaimKey -> idx.toString)),
          routes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Single)
        )
      }

    }

    "show an error summary" when {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.selectBasisForClaimSubmit(JourneyBindable.Single)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "the user does not select an option" in {

        val basisOfClaimAnswer = BasisOfClaimAnswer.EndUseRelief.some

        val draftC285Claim                = sessionWithClaimState(basisOfClaimAnswer)._3.copy(basisOfClaimAnswer = basisOfClaimAnswer)
        val (session, fillingOutClaim, _) = sessionWithClaimState(basisOfClaimAnswer)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq.empty
          ),
          messageFromMessageKey(s"$selectBasisForClaimKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$selectBasisForClaimKey.error.required"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in {

        val basisOfClaimAnswer = BasisOfClaimAnswer.EndUseRelief.some

        val draftC285Claim =
          sessionWithClaimState(basisOfClaimAnswer)._3.copy(basisOfClaimAnswer = basisOfClaimAnswer)

        val (session, fillingOutClaim, _) = sessionWithClaimState(basisOfClaimAnswer)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(Seq(selectBasisForClaimKey -> "blah")),
          messageFromMessageKey(s"$selectBasisForClaimKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$selectBasisForClaimKey.error.number"
            ),
          BAD_REQUEST
        )
      }

    }
  }

}
