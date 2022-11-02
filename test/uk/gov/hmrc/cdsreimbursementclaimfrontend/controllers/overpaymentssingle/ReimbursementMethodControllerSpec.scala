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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswerList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.arbitrarySupportingEvidenceAnswerList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes

class ReimbursementMethodControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: ReimbursementMethodController = instanceOf[ReimbursementMethodController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  "Reimbursement Method Controller" must {

    def performAction: Future[Result] = controller.show(FakeRequest())

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction,
          baseRoutes.StartController.start()
        )

      }
    }

    "display the page" when {
      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("reimbursement-method.title"),
          doc => {
            isCurrentMonthAjustmentChecked(doc) shouldBe false
            isBankTransferChecked(doc)          shouldBe false
          }
        )
      }

      "the user has answered this question before and chooses Current Month Adjustment Option" in {
        val session = getSessionWithPreviousAnswer(CurrentMonthAdjustment)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("reimbursement-method.title"),
          doc => {
            isCurrentMonthAjustmentChecked(doc) shouldBe true
            isBankTransferChecked(doc)          shouldBe false
          }
        )
      }

      "the user has answered this question before and chooses Bank Account Transfer Option" in {
        val session = getSessionWithPreviousAnswer(BankAccountTransfer)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey("reimbursement-method.title"),
          doc => {
            isCurrentMonthAjustmentChecked(doc) shouldBe false
            isBankTransferChecked(doc)          shouldBe true
          }
        )
      }
    }

    "handle submit requests" when {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "User chooses the Current Month Adjustment Option" in {
        val initialSession = session
        val updatedSession = updateSession(initialSession, CurrentMonthAdjustment)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(ReimbursementMethodController.reimbursementMethodKey -> "0")),
          OverpaymentsRoutes.ChooseFileTypeController.show(JourneyBindable.Single)
        )
      }

      "User chooses the Bank Account Transfer Option" in {
        val initialSession = session
        val updatedSession = updateSession(initialSession, BankAccountTransfer)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(ReimbursementMethodController.reimbursementMethodKey -> "1")),
          OverpaymentsRoutes.BankAccountController.checkBankAccountDetails(JourneyBindable.Single)
        )
      }

      "the user amends their previous answer from BTA to CMA via the cya page" in {
        val session        = includeSupportingEvidenceInSession(getSessionWithPreviousAnswer(BankAccountTransfer))
        val updatedSession = updateSession(session, CurrentMonthAdjustment)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(ReimbursementMethodController.reimbursementMethodKey -> "0")),
          OverpaymentsRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Single)
        )
      }

      "the user amends their previous answer from CMA to BTA via the cya page" in {
        val session        = includeSupportingEvidenceInSession(getSessionWithPreviousAnswer(CurrentMonthAdjustment))
        val updatedSession = updateSession(session, BankAccountTransfer)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(ReimbursementMethodController.reimbursementMethodKey -> "1")),
          OverpaymentsRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Single)
        )
      }
    }

    "show an error summary" when {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "the user does not select an option and submits the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(Seq.empty),
          messageFromMessageKey("reimbursement-method.title"),
          doc => {
            isCurrentMonthAjustmentChecked(doc) shouldBe false
            isBankTransferChecked(doc)          shouldBe false
            getErrorSummary(doc)                shouldBe messageFromMessageKey("reimbursement-method.error.required")
          },
          BAD_REQUEST
        )
      }
    }
  }

  private def session: SessionData = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, DraftClaim.blank)

    SessionData.empty.copy(
      journeyStatus = Some(journey)
    )
  }

  private def getSessionWithPreviousAnswer(reimbursementMethod: ReimbursementMethod): SessionData = {
    val draftC285Claim      = DraftClaim.blank.copy(reimbursementMethodAnswer = Some(reimbursementMethod))
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    SessionData.empty.copy(journeyStatus = Some(journey))
  }

  private def isCurrentMonthAjustmentChecked(document: Document): Boolean =
    isChecked(document, "reimbursement-method-cma")
  private def isBankTransferChecked(document: Document): Boolean          =
    isChecked(document, "reimbursement-method-bank-transfer")

  private def updateSession(sessionData: SessionData, reimbusementMethod: ReimbursementMethod): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      =
          draftClaim.copy(reimbursementMethodAnswer = Some(reimbusementMethod))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }

  private def includeSupportingEvidenceInSession(sessionData: SessionData): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      =
          draftClaim.copy(supportingEvidencesAnswer = Some(sample[SupportingEvidencesAnswerList]))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }
}
