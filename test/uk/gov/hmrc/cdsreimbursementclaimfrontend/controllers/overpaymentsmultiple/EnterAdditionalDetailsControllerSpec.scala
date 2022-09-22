/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.additionalDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AdditionalDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen.genValidDraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}

import scala.concurrent.Future

class EnterAdditionalDetailsControllerSpec
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
    ("ClaimType", "JourneyBindable"),
    (TypeOfClaimAnswer.Multiple, JourneyBindable.Multiple)
  )

  lazy val controller: EnterAdditionalDetailsController = instanceOf[EnterAdditionalDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeAdditionalDetailsAnswer: Option[AdditionalDetailsAnswer],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) =
    sessionWithClaim(
      DraftClaim.blank.copy(
        additionalDetailsAnswer = maybeAdditionalDetailsAnswer,
        typeOfClaim = maybeTypeOfClaim
      )
    )

  private def sessionWithClaim(draftC285Claim: DraftClaim): (SessionData, FillingOutClaim, DraftClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)

    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftC285Claim
    )
  }

  "Enter Additional Details Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in forAll(testCases) { (numberOfClaims, _) =>
        def performAction(): Future[Result] = controller.show(FakeRequest())

        val (session, _, _) = sessionWithClaimState(None, Some(numberOfClaims))

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

      "the user has not answered this question before" in forAll(testCases) { (numberOfClaims, _) =>
        def performAction(): Future[Result] = controller.show(FakeRequest())

        val draftC285Claim                = sessionWithClaimState(None, Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfOverpaymentClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MRN])
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(None, Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-additional-details.title")
        )
      }

      "the user has answered this question before" in forAll(testCases) { (numberOfClaims, _) =>
        def performAction(): Future[Result] = controller.show(FakeRequest())

        val answers = AdditionalDetailsAnswer("some package")

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfOverpaymentClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MRN])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-additional-details.title")
        )
      }

      "the user has come from the CYA page and is amending their answer" in forAll(testCases) { (numberOfClaims, _) =>
        def performAction(): Future[Result] = controller.show(FakeRequest())

        val answers = AdditionalDetailsAnswer("some package")

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfOverpaymentClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MRN])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-additional-details.title")
        )
      }
    }

    "handle submit requests" when {

      "user enters some details" in forAll(testCases) { (numberOfClaims, _) =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.submit(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = AdditionalDetailsAnswer("some package")

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfOverpaymentClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MRN])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq("enter-additional-details" -> "some package")),
          overpaymentsMultipleRoutes.SelectMultipleDutiesController.selectDuties(index = 1)
        )
      }

      "the user amends their answer" in {

        val journey     = JourneyBindable.Single
        val typeOfClaim = toTypeOfClaim(journey)

        val claim = sample(genValidDraftClaim(typeOfClaim))

        val (session, fillingOutClaim, _) = sessionWithClaim(claim)

        val updatedJourney = fillingOutClaim.copy(draftClaim = claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          controller.submit(
            FakeRequest().withFormUrlEncodedBody(
              "enter-additional-details" -> claim.additionalDetailsAnswer.fold("")(_.value)
            )
          ),
          OverpaymentsRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey)
        )
      }

    }

    "show an error summary" when {

      "the user enters more than 500 characters" in forAll(testCases) { (numberOfClaims, _) =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.submit(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = AdditionalDetailsAnswer(List.fill(600)('c').mkString(" "))

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfOverpaymentClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MRN])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(Seq("enter-additional-details" -> List.fill(600)('c').mkString(" "))),
          messageFromMessageKey("enter-additional-details.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-additional-details.error.maxLength"
            ),
          BAD_REQUEST
        )
      }

    }
  }

  "Form Validation" must {
    val form              = additionalDetailsForm
    val additionalDetails = "enter-additional-details"
    val goodData          = Map(
      additionalDetails -> "A box of biscuits"
    )

    "accept good additional details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "additional details" should {
      "Accept longest possible details" in {
        val errors = form.bind(goodData.updated(additionalDetails, List.fill(500)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject details when it's too long" in {
        val errors = form.bind(goodData.updated(additionalDetails, List.fill(501)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }
  }
}
