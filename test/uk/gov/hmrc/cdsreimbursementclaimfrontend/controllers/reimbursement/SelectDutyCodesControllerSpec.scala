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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType.{CiderPerry, EuDuty, UkDuty}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyCodesAnswer, DutyType, DutyTypesAnswer, ReimbursementClaim, ReimbursementClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, TaxCode}

import scala.concurrent.Future

class SelectDutyCodesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectDutyCodesController = instanceOf[SelectDutyCodesController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithDutyCodesState(
    maybeDutyTypesSelectedAnswer: Option[DutyTypesAnswer],
    maybeDutyCodesSelectedAnswer: Option[DutyCodesAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        dutyTypesSelectedAnswer = maybeDutyTypesSelectedAnswer,
        dutyCodesSelectedAnswer = maybeDutyCodesSelectedAnswer
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

  private def updateSession(sessionData: SessionData, dutyCodesAnswer: DutyCodesAnswer): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftC285Claim)) =>
        val newClaim      = draftClaim.copy(
          dutyCodesSelectedAnswer = Some(dutyCodesAnswer),
          reimbursementClaimAnswer = Some(
            ReimbursementClaimAnswer(
              Map(
                UkDuty     -> Map(
                  TaxCode.A20 -> ReimbursementClaim(paidAmount = 600.00, shouldOfPaid = 300.00),
                  TaxCode.A00 -> ReimbursementClaim(paidAmount = 1000.00, shouldOfPaid = 400.00)
                ),
                EuDuty     -> Map(
                  TaxCode.A70 -> ReimbursementClaim(paidAmount = 600.00, shouldOfPaid = 300.00),
                  TaxCode.A00 -> ReimbursementClaim(paidAmount = 1000.00, shouldOfPaid = 400.00)
                ),
                CiderPerry -> Map(
                  TaxCode.NI431 -> ReimbursementClaim(paidAmount = 600.00, shouldOfPaid = 300.00)
                )
              )
            )
          )
        )
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                       => fail()
    }

  val messageKey = "select-duty-codes"

  "Select Duty Codes Controller" must {

    def performAction(dutyType: DutyType): Future[Result] = controller.showDutyCodes(dutyType)(FakeRequest())

    def performActionWithFormData(dutyType: DutyType, data: Seq[(String, String)]): Future[Result] =
      controller.submitDutyCodes(dutyType)(
        FakeRequest().withFormUrlEncodedBody(data: _*)
      )

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        val session = SessionData.empty.copy(journeyStatus = None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(DutyType.UkDuty),
          baseRoutes.StartController.start()
        )

      }
    }

    "redirect to the duty types page" when {

      "the user has not selected any duty types" in {

        def performStartAction(): Future[Result] = controller.start()(FakeRequest())

        val (session, _, _) = sessionWithDutyCodesState(None, None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performStartAction(),
          routes.SelectDutyTypesController.showDutyTypes()
        )

      }
    }

    "display the page" when {

      "the user has not answered this question before for a particular duty code" in {

        val (session, _, _) = sessionWithDutyCodesState(None, None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(DutyType.UkDuty),
          messageFromMessageKey("select-duty-codes.title", messageFromMessageKey("select-duty-codes.h1.uk-duty")),
          doc =>
            TaxCode.listOfUKTaxCodes.map { taxCode =>
              isCheckboxChecked(doc, taxCode.value) shouldBe false
            }
        )
      }

      "the user has answered this question before with the previously selected duty code checked" in {

        val (session, _, _) =
          sessionWithDutyCodesState(
            Some(DutyTypesAnswer(List(DutyType.UkDuty))),
            Some(DutyCodesAnswer(Map(DutyType.UkDuty -> List(TaxCode.A00))))
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(DutyType.UkDuty),
          messageFromMessageKey("select-duty-codes.title", messageFromMessageKey("select-duty-codes.h1.uk-duty")),
          doc => isCheckboxChecked(doc, "A00") shouldBe true
        )
      }

    }

    "handle submit requests" when {

      "user has never answered this question before for a particular duty type" in {

        val (session, _, _) =
          sessionWithDutyCodesState(
            Some(DutyTypesAnswer(List(DutyType.UkDuty))),
            Some(DutyCodesAnswer(Map(DutyType.UkDuty -> List.empty)))
          )

        val updatedSession: SessionData =
          updateSession(session, DutyCodesAnswer(Map(DutyType.UkDuty -> List(TaxCode.A00))))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performActionWithFormData(DutyType.UkDuty, Seq(s"$messageKey[]" -> "A00")),
          routes.CheckReimbursementClaimController.showReimbursementClaim()
//          routes.EnterReimbursementClaimController.start()
        )

      }

      "user has selected only one duty type and has selected a duty code" must {

        "redirect to the paid and claim amount page" in {

          val (session, _, _) =
            sessionWithDutyCodesState(
              Some(DutyTypesAnswer(List(DutyType.UkDuty))),
              Some(DutyCodesAnswer(Map(DutyType.UkDuty -> List.empty)))
            )

          val updatedSession: SessionData =
            updateSession(session, DutyCodesAnswer(Map(DutyType.UkDuty -> List(TaxCode.A00))))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performActionWithFormData(DutyType.UkDuty, Seq(s"$messageKey[]" -> "A00")),
            routes.CheckReimbursementClaimController.showReimbursementClaim()
//            routes.EnterReimbursementClaimController.start()
          )
        }

      }

      "user has specified duty codes for all duty types" in {

        val (session, _, _) =
          sessionWithDutyCodesState(
            Some(DutyTypesAnswer(List(DutyType.UkDuty, DutyType.EuDuty))),
            Some(DutyCodesAnswer(Map(DutyType.UkDuty -> List(TaxCode.A00), DutyType.EuDuty -> List.empty)))
          )

        val updatedSession: SessionData =
          updateSession(
            session,
            DutyCodesAnswer(Map(DutyType.UkDuty -> List(TaxCode.A00), DutyType.EuDuty -> List(TaxCode.A50)))
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performActionWithFormData(DutyType.EuDuty, Seq(s"$messageKey[]" -> "A50")),
          routes.CheckReimbursementClaimController.showReimbursementClaim()
//          routes.EnterReimbursementClaimController.start()
        )

      }

      "user has not specified duty codes for all duty types and so the user is directed to the next duty code page" in {

        val (session, _, _) =
          sessionWithDutyCodesState(
            Some(DutyTypesAnswer(List(DutyType.UkDuty, DutyType.EuDuty, DutyType.Beer))),
            Some(
              DutyCodesAnswer(
                Map(DutyType.UkDuty -> List.empty, DutyType.EuDuty -> List.empty, DutyType.Beer -> List.empty)
              )
            )
          )

        val updatedSession: SessionData =
          updateSession(
            session,
            DutyCodesAnswer(
              Map(DutyType.UkDuty -> List(TaxCode.A00), DutyType.EuDuty -> List.empty, DutyType.Beer -> List.empty)
            )
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performActionWithFormData(DutyType.UkDuty, Seq(s"$messageKey[]" -> "A00")),
          routes.SelectDutyCodesController.showDutyCodes(DutyType.EuDuty)
        )

      }

      "user must be directed to next duty code page in the order specified in the duty types page " in {

        val (session, _, _) =
          sessionWithDutyCodesState(
            Some(DutyTypesAnswer(List(DutyType.UkDuty, DutyType.Beer, DutyType.Spirits))),
            Some(
              DutyCodesAnswer(
                Map(DutyType.UkDuty -> List.empty, DutyType.Beer -> List.empty, DutyType.Spirits -> List.empty)
              )
            )
          )

        val updatedSession: SessionData =
          updateSession(
            session,
            DutyCodesAnswer(
              Map(DutyType.UkDuty -> List(TaxCode.A00), DutyType.Beer -> List.empty, DutyType.Spirits -> List.empty)
            )
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performActionWithFormData(DutyType.UkDuty, Seq(s"$messageKey[]" -> "A00")),
          routes.SelectDutyCodesController.showDutyCodes(DutyType.Beer)
        )

      }

    }

    "show an error summary" when {

      "no duty code is selected" in {

        val (session, _, _) =
          sessionWithDutyCodesState(
            Some(DutyTypesAnswer(List(DutyType.UkDuty))),
            Some(
              DutyCodesAnswer(
                Map(DutyType.UkDuty -> List.empty)
              )
            )
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionWithFormData(DutyType.UkDuty, Seq.empty),
          messageFromMessageKey("select-duty-codes.title", messageFromMessageKey("select-duty-codes.h1.uk-duty")),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"select-duty-codes.error.required"
            ),
          BAD_REQUEST
        )

      }

    }

  }

}
