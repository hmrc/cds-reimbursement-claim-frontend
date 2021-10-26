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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutyType, ReimbursementClaim, SessionData, SignedInUserDetails, TaxCode}

import scala.concurrent.Future

class SelectDutyTypesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectDutyTypesController = instanceOf[SelectDutyTypesController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithDutyTypesState(
    maybeDutyTypesSelectedAnswer: Option[DutyTypesAnswer],
    maybeDutyCodesSelectedAnswer: Option[DutyCodesAnswer] = None,
    reimbursementClaimAnswer: Option[ReimbursementClaimAnswer] = None
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        dutyTypesSelectedAnswer = maybeDutyTypesSelectedAnswer,
        dutyCodesSelectedAnswer = maybeDutyCodesSelectedAnswer,
        reimbursementClaimAnswer = reimbursementClaimAnswer
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

  def performAction(data: Seq[(String, String)]): Future[Result] =
    controller.submitDutyTypes()(
      FakeRequest().withFormUrlEncodedBody(data: _*)
    )

  "Select Duty Types Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.showDutyTypes()(FakeRequest())

        val session = SessionData.empty.copy(journeyStatus = None)

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

      def performAction(): Future[Result] = controller.showDutyTypes()(FakeRequest())

      "the user has not answered this question before" in {

        val (session, _, _) = sessionWithDutyTypesState(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duty-types.title")
        )
      }

      "the user has answered this question before with the previously selected duty types checked" in {

        val (session, _, _) = sessionWithDutyTypesState(Some(DutyTypesAnswer(List(DutyType.UkDuty))))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duty-types.title"),
          doc => isCheckboxChecked(doc, "uk-duty") shouldBe true
        )
      }

    }

    "handle submit requests" when {

      "user has never answered this question before" in {

        val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(
            Seq("select-duty-types[0]" -> "uk-duty")
          ),
          routes.SelectDutyCodesController.start()
        )
      }

      "user does change his selection" in {

        val answers = DutyTypesAnswer(List(DutyType.EuDuty))

        val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(
            Seq("select-duty-types[0]" -> "uk-duty")
          ),
          routes.SelectDutyCodesController.start()
        )
      }

      "user does not change his selection" in {

        val answers = DutyTypesAnswer(List(DutyType.UkDuty))

        val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(
            Seq("select-duty-types[0]" -> "uk-duty")
          ),
          routes.SelectDutyCodesController.start()
        )
      }

      "user deletes a duty type and all other duty types have associated duty codes and claims" in {

        val dutyTypesAnswer          = DutyTypesAnswer(List(DutyType.UkDuty, DutyType.EuDuty))
        val dutyCodesAnswer          =
          DutyCodesAnswer(Map(DutyType.UkDuty -> List(TaxCode.A00), DutyType.EuDuty -> List(TaxCode.A50)))
        val reimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map(
            DutyType.UkDuty -> Map(TaxCode.A00 -> ReimbursementClaim(10, 2)),
            DutyType.EuDuty -> Map(TaxCode.A50 -> ReimbursementClaim(10, 2))
          )
        )

        val (session, fillingOutClaim, draftC285Claim) =
          sessionWithDutyTypesState(Some(dutyTypesAnswer), Some(dutyCodesAnswer), Some(reimbursementClaimAnswer))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(
            Seq("select-duty-types[0]" -> "eu-duty")
          ),
          routes.SelectDutyCodesController.start()
        )
      }

    }

    "show an error summary" when {

      "no duty type is selected" in {

        val answers = DutyTypesAnswer(List(DutyType.UkDuty))

        val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq()
          ),
          messageFromMessageKey("select-duty-types.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-duty-types.error.required"
            ),
          BAD_REQUEST
        )
      }

    }

  }

}
