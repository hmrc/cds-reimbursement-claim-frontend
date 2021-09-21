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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, CommodityDetails, MovementReferenceNumber, SelectNumberOfClaimsAnswer, SessionData, SignedInUserDetails, upscan => _}

import scala.concurrent.Future

class EnterCommoditiesDetailsControllerSpec
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
    ("NumberOfClaimsType", "JourneyBindable"),
    (SelectNumberOfClaimsAnswer.Individual, JourneyBindable.Single),
    (SelectNumberOfClaimsAnswer.Multiple, JourneyBindable.Multiple),
    (SelectNumberOfClaimsAnswer.Scheduled, JourneyBindable.Scheduled)
  )

  lazy val controller: EnterCommoditiesDetailsController = instanceOf[EnterCommoditiesDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeCommoditiesDetailsAnswer: Option[CommodityDetails],
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim
        .copy(
          commoditiesDetailsAnswer = maybeCommoditiesDetailsAnswer,
          selectNumberOfClaimsAnswer = numberOfClaims
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

  "Enter Commodities Details Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(): Future[Result] = controller.enterCommoditiesDetails(journeyBindable)(FakeRequest())

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

      "the user has not answered this question before" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(): Future[Result] = controller.enterCommoditiesDetails(journeyBindable)(FakeRequest())

        val draftC285Claim                = sessionWithClaimState(None, Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MovementReferenceNumber])
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(None, Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-commodities-details.title")
        )
      }

      "the user has answered this question before" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(): Future[Result] = controller.enterCommoditiesDetails(journeyBindable)(FakeRequest())

        val answers = CommodityDetails("some package")

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MovementReferenceNumber])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-commodities-details.title")
        )
      }

      "the user has come from the CYA page and is amending their answer" in forAll(testCases) {
        (numberOfClaims, journeyBindable) =>
          def performAction(): Future[Result] = controller.changeCommoditiesDetails(journeyBindable)(FakeRequest())

          val answers = CommodityDetails("some package")

          val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
            .copy(
              basisOfClaimAnswer = Some(BasisOfClaim.DutySuspension),
              movementReferenceNumber = Some(sample[MovementReferenceNumber])
            )

          val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

          val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-commodities-details.title")
          )
      }
    }

    "handle submit requests" when {

      "user enters some details" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterCommoditiesDetailsSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = CommodityDetails("some package")

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MovementReferenceNumber])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq("enter-commodities-details" -> "some package")),
          if (journeyBindable === Scheduled) {
            reimbursementRoutes.SelectDutyTypesController.showDutyTypes()
          } else {
            routes.SelectDutiesController.selectDuties()
          }
        )
      }

      "the user amends their answer" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.changeCommoditiesDetailsSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = CommodityDetails("some package")

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MovementReferenceNumber])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(Seq("enter-commodities-details" -> "some package")),
          routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
        )
      }

    }

    "show an error summary" when {

      "the user enters more than 500 characters" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterCommoditiesDetailsSubmit(journeyBindable)(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val answers = CommodityDetails(List.fill(600)('c').mkString(" "))

        val draftC285Claim = sessionWithClaimState(Some(answers), Some(numberOfClaims))._3
          .copy(
            basisOfClaimAnswer = Some(BasisOfClaim.DutySuspension),
            movementReferenceNumber = Some(sample[MovementReferenceNumber])
          )

        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers), Some(numberOfClaims))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(Seq("enter-commodities-details" -> List.fill(600)('c').mkString(" "))),
          messageFromMessageKey("enter-commodities-details.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-commodities-details.error.maxLength"
            ),
          BAD_REQUEST
        )
      }

    }
  }

  "Form Validation" must {
    val form             = EnterCommoditiesDetailsController.commoditiesDetailsForm
    val commodityDetails = "enter-commodities-details"
    val goodData         = Map(
      commodityDetails -> "A box of biscuits"
    )

    "accept good commodity details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "commodity details" should {
      "Accept longest possible details" in {
        val errors = form.bind(goodData.updated(commodityDetails, List.fill(500)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject details when it's too long" in {
        val errors = form.bind(goodData.updated(commodityDetails, List.fill(501)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }
  }
}
