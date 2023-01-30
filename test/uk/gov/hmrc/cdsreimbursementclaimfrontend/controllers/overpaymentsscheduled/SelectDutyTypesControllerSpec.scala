/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

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

  val controller: SelectDutyTypesController = instanceOf[SelectDutyTypesController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  def performAction(data: Seq[(String, String)]): Future[Result] =
    controller.submitDutyTypes(FakeRequest().withFormUrlEncodedBody(data: _*))

  "display the page" when {

    def performAction(): Future[Result] = controller.showDutyTypes(FakeRequest())

    "the user has not answered this question before" in {
      val (session, _, _) = sessionWithDutyTypesState(None)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey("select-duty-types.title"),
        doc => {
          selectedCheckBox(doc)                                    shouldBe empty
          doc.getElementById("select-duty-types").`val`()          shouldBe "uk-duty"
          doc.getElementById("select-duty-types-2").`val`()        shouldBe "eu-duty"
          doc.getElementById("select-duty-types-excise").`val`()   shouldBe "beer"
          doc.getElementById("select-duty-types-excise-2").`val`() shouldBe "wine"
        }
      )
    }

    "the user has answered this question before with the previously selected duty types checked" in forAll {
      duty: DutyType =>
        val (session, _, _) = sessionWithDutyTypesState(
          Some(
            SelectedDutyTaxCodesReimbursementAnswer
              .buildFrom(List(duty))
              .synchronizingWith(SelectedDutyTaxCodesReimbursementAnswer.none)
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duty-types.title"),
          doc => isCheckboxChecked(doc, duty.repr) shouldBe true
        )
    }
  }

  "handle submit requests" when {

    "user has never answered this question before" in forAll { duty: DutyType =>
      val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(None)

      val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        performAction(Seq("select-duty-types[0]" -> duty.repr)),
        overpaymentsScheduledRoutes.SelectDutyCodesController.iterate()
      )
    }

    "user does change his selection" in forAll { duty: DutyType =>
      val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(None)

      val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        performAction(Seq("select-duty-types[0]" -> duty.repr)),
        overpaymentsScheduledRoutes.SelectDutyCodesController.iterate()
      )
    }

    "user does not change his selection" in forAll { duty: DutyType =>
      val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(
        Some(
          SelectedDutyTaxCodesReimbursementAnswer
            .buildFrom(List(duty))
            .synchronizingWith(SelectedDutyTaxCodesReimbursementAnswer.none)
        )
      )

      val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
      }

      checkIsRedirect(
        performAction(
          Seq("select-duty-types[0]" -> duty.repr)
        ),
        overpaymentsScheduledRoutes.SelectDutyCodesController.iterate()
      )
    }
  }

  "show an error summary" when {
    "no duty type is selected" in {
      val (session, fillingOutClaim, draftC285Claim) = sessionWithDutyTypesState(None)

      val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
      }

      checkPageIsDisplayed(
        performAction(Nil),
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

  def sessionWithDutyTypesState(
    selectedDutyTaxCodesReimbursementAnswer: Option[SelectedDutyTaxCodesReimbursementAnswer] = None
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(selectedDutyTaxCodesReimbursementAnswer = selectedDutyTaxCodesReimbursementAnswer)
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftC285Claim
    )
  }
}
