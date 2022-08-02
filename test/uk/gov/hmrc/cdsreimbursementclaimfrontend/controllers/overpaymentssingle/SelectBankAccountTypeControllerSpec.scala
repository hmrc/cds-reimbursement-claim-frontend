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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.Future

class SelectBankAccountTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val messageKey = "select-bank-account-type"

  lazy val controller: SelectBankAccountTypeController = instanceOf[SelectBankAccountTypeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def updateSession(sessionData: SessionData, bankAccountType: BankAccountType): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      = draftClaim.copy(bankAccountTypeAnswer = Some(bankAccountType))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }

  def isBusinessChecked(document: Document): Boolean =
    isChecked(document, s"$messageKey-business-bank-account")

  def isPersonalChecked(document: Document): Boolean =
    isChecked(document, s"$messageKey-personal-bank-account")

  "SelectBackAccountTypeController" must {
    "redirect to the start page if no journey present" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(SessionData.empty)
      }

      checkIsRedirect(
        controller.show()(FakeRequest()),
        baseRoutes.StartController.start()
      )
    }

    "display the page" when {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "the user has not answered this question before" in forAll {
        (fullDraftClaim: DraftClaim, ggCredId: GGCredId, signedInUserDetails: SignedInUserDetails) =>
          val draftClaim = fullDraftClaim.copy(bankAccountTypeAnswer = None)
          val journey    = FillingOutClaim(ggCredId, signedInUserDetails, draftClaim)
          val session    = SessionData.empty.copy(journeyStatus = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messageKey.title"),
            doc => {
              formAction(doc)        shouldBe routes.SelectBankAccountTypeController.submit.url
              isBusinessChecked(doc) shouldBe false
              isPersonalChecked(doc) shouldBe false
            }
          )
      }

      "the user has answered this question before" in forAll {
        (draftClaim: DraftClaim, ggCredId: GGCredId, signedInUserDetails: SignedInUserDetails) =>
          val journey = FillingOutClaim(ggCredId, signedInUserDetails, draftClaim)
          val session = SessionData.empty.copy(journeyStatus = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messageKey.title"),
            doc => {
              isBusinessChecked(doc) shouldBe draftClaim.bankAccountTypeAnswer.contains(BankAccountType.Business)
              isPersonalChecked(doc) shouldBe draftClaim.bankAccountTypeAnswer.contains(BankAccountType.Personal)
            }
          )
      }
    }

    "handle submit requests" when {
      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "user chooses a Bank Account Type" in forAll {
        (
          fullDraftClaim: DraftClaim,
          ggCredId: GGCredId,
          signedInUserDetails: SignedInUserDetails,
          bankAccountType: BankAccountType
        ) =>
          val draftClaim    = fullDraftClaim.copy(bankAccountTypeAnswer = None)
          val journey       = FillingOutClaim(ggCredId, signedInUserDetails, draftClaim)
          val sessionBefore = SessionData.empty.copy(journeyStatus = Some(journey))
          val sessionAfter  = updateSession(sessionBefore, bankAccountType)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionBefore)
            mockStoreSession(sessionAfter)(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("select-bank-account-type" -> bankAccountType.toString)),
            OverpaymentsRoutes.BankAccountController.enterBankAccountDetails(JourneyBindable.Single)
          )
      }

      "user chooses a Bank Account Type but something goes wrong updating the session" in forAll {
        (
          fullDraftClaim: DraftClaim,
          ggCredId: GGCredId,
          signedInUserDetails: SignedInUserDetails,
          bankAccountType: BankAccountType
        ) =>
          val draftClaim    = fullDraftClaim.copy(bankAccountTypeAnswer = None)
          val journey       = FillingOutClaim(ggCredId, signedInUserDetails, draftClaim)
          val sessionBefore = SessionData.empty.copy(journeyStatus = Some(journey))
          val sessionAfter  = updateSession(sessionBefore, bankAccountType)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionBefore)
            mockStoreSession(sessionAfter)(Left(Error("Something went wrong")))
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("select-bank-account-type" -> bankAccountType.toString))
          )
      }

      "the user amends their previous answer" in forAll {
        (draftClaim: DraftClaim, ggCredId: GGCredId, signedInUserDetails: SignedInUserDetails) =>
          val bankAccountType: BankAccountType = draftClaim.bankAccountTypeAnswer match {
            case Some(BankAccountType.Personal)     => BankAccountType.Business
            case Some(BankAccountType.Business) | _ => BankAccountType.Personal
          }

          val journey       = FillingOutClaim(ggCredId, signedInUserDetails, draftClaim)
          val sessionBefore = SessionData.empty.copy(journeyStatus = Some(journey))
          val sessionAfter  = updateSession(sessionBefore, bankAccountType)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionBefore)
            mockStoreSession(sessionAfter)(Right(()))
          }

          checkIsRedirect(
            performAction(Seq("select-bank-account-type" -> bankAccountType.toString)),
            OverpaymentsRoutes.BankAccountController.enterBankAccountDetails(JourneyBindable.Single)
          )
      }

      "the user does not select an option and submits the page" in forAll {
        (fullDraftClaim: DraftClaim, ggCredId: GGCredId, signedInUserDetails: SignedInUserDetails) =>
          val draftClaim = fullDraftClaim.copy(bankAccountTypeAnswer = None)
          val journey    = FillingOutClaim(ggCredId, signedInUserDetails, draftClaim)
          val session    = SessionData.empty.copy(journeyStatus = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSessionNotCalled
          }

          checkPageIsDisplayed(
            performAction(Seq()),
            messageFromMessageKey(s"$messageKey.title"),
            doc => {
              formAction(doc)      shouldBe routes.SelectBankAccountTypeController.submit.url
              getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messageKey.error.required")
            },
            expectedStatus = BAD_REQUEST
          )
      }
    }
  }
}
