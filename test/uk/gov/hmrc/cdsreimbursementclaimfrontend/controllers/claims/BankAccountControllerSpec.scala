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

import cats.data.EitherT
import cats.implicits._
import org.jsoup.Jsoup
import org.scalamock.scalatest.MockFactory
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.{AccountName, AccountNumber, BankAccountDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.routes.SupportingEvidenceController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.CompleteBankAccountDetailAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.{BarsBusinessAssessRequest, BarsPersonalAssessRequest}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.{Indeterminate, No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.{CommonBarsResponse, ReputationErrorResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetailsAnswer, Error, SessionData, SignedInUserDetails, SortCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BankAccountControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with MockFactory {

  val claimService = mock[ClaimService]

  def mockBusinessReputation(response: Either[Error, CommonBarsResponse]) =
    (claimService
      .getBusinessAccountReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .atLeastOnce()

  def mockPersonalReputation(response: Either[Error, CommonBarsResponse]) =
    (claimService
      .getPersonalAccountReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .atLeastOnce()

  lazy val controller: BankAccountController = instanceOf[BankAccountController]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionStore),
      bind[ClaimService].toInstance(claimService)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeBankAccountDetails: Option[BankAccountDetailsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(bankAccountDetailsAnswer = maybeBankAccountDetails)
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

  "Bank Account Controller" when {

    "Business Bank Account" should {

      val businessBankAccount = BankAccountDetails(
        accountName = AccountName("Some Account"),
        isBusinessAccount = List(0),
        sortCode = SortCode("123456"),
        accountNumber = AccountNumber("12345678")
      )

      "Let users through when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid" in {
        val businessResponse =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = None, otherError = None)
        mockBusinessReputation(Right(businessResponse))
        val answers          = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val errors = doc.getElementsByClass("error-summary-list").select("li").asScala
        errors.size shouldBe 0

        checkIsRedirect(result, SupportingEvidenceController.uploadSupportingEvidence())
      }

      "Let users through when the Bank Account Validation succeeds with accountExists" in {
        val businessResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Indeterminate,
          accountExists = Some(Yes),
          otherError = None
        )
        mockBusinessReputation(Right(businessResponse))
        val answers          = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val errors = doc.getElementsByClass("error-summary-list").select("li").asScala
        errors.size shouldBe 0

        checkIsRedirect(result, SupportingEvidenceController.uploadSupportingEvidence())
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = No" in {
        val businessResponse =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = No, accountExists = None, otherError = None)
        mockBusinessReputation(Right(businessResponse))
        val answers          = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = doc.getElementsByClass("error-summary-list").select("li").text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail when the Bank Account Validation fails with accountExists = No" in {
        val businessResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Indeterminate,
          accountExists = Some(No),
          otherError = None
        )
        mockBusinessReputation(Right(businessResponse))
        val answers          = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = doc.getElementsByClass("error-summary-list").select("li").text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail when the Bank Account Number" in {
        val errorResponse    =
          ReputationErrorResponse(code = "INVALID_ACCOUNT_NUMBER", desc = "123456: invalid account number")
        val businessResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = No,
          accountExists = None,
          otherError = Some(errorResponse)
        )
        mockBusinessReputation(Right(businessResponse))
        val answers          = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = doc.getElementsByClass("error-summary-list").select("li").text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.INVALID_ACCOUNT_NUMBER")
        status(result) shouldBe BAD_REQUEST
      }

    }

    "Personal Bank Account" should {

      val personalBankAccount = BankAccountDetails(
        accountName = AccountName("Some Account"),
        isBusinessAccount = Nil,
        sortCode = SortCode("123456"),
        accountNumber = AccountNumber("12345678")
      )

      "Let users through when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid" in {
        val personalResponse =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = None, otherError = None)
        mockPersonalReputation(Right(personalResponse))
        val answers          = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val errors = doc.getElementsByClass("error-summary-list").select("li").asScala
        errors.size shouldBe 0

        checkIsRedirect(result, SupportingEvidenceController.uploadSupportingEvidence())
      }

      "Let users through when the Bank Account Validation succeeds with accountExists" in {
        val personalResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Indeterminate,
          accountExists = Some(Yes),
          otherError = None
        )
        mockPersonalReputation(Right(personalResponse))
        val answers          = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val errors = doc.getElementsByClass("error-summary-list").select("li").asScala
        errors.size shouldBe 0

        checkIsRedirect(result, SupportingEvidenceController.uploadSupportingEvidence())
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = No" in {
        val personalResponse =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = No, accountExists = None, otherError = None)
        mockPersonalReputation(Right(personalResponse))
        val answers          = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = doc.getElementsByClass("error-summary-list").select("li").text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail when the Bank Account Validation fails with accountExists = No" in {
        val personalResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Indeterminate,
          accountExists = Some(No),
          otherError = None
        )
        mockPersonalReputation(Right(personalResponse))
        val answers          = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = doc.getElementsByClass("error-summary-list").select("li").text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail when the Sort Code is invalid" in {
        val errorResponse    = ReputationErrorResponse(code = "INVALID_SORTCODE", desc = "1234: invalid sortcode")
        val personalResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = No,
          accountExists = None,
          otherError = Some(errorResponse)
        )
        mockPersonalReputation(Right(personalResponse))
        val answers          = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = doc.getElementsByClass("error-summary-list").select("li").text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.INVALID_SORTCODE")
        status(result) shouldBe BAD_REQUEST
      }

    }

  }
}
