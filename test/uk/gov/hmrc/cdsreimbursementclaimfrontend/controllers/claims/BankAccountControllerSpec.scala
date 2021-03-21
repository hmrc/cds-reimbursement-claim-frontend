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
import org.jsoup.nodes.Document
import org.scalamock.scalatest.MockFactory
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.data.Forms.{boolean, optional}
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.{CommonBarsResponse, ReputationErrorResponse, ReputationResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetailsAnswer, Error, SessionData, SignedInUserDetails, SortCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators._

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
      .once()

  def mockPersonalReputation(response: Either[Error, CommonBarsResponse]) =
    (claimService
      .getPersonalAccountReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  lazy val controller: BankAccountController = instanceOf[BankAccountController]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionStore),
      bind[ClaimService].toInstance(claimService)
    )

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

  private def updateSession(sessionData: SessionData, bankAccountDetails: BankAccountDetails): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
        val newClaim      =
          draftClaim.copy(bankAccountDetailsAnswer = Some(CompleteBankAccountDetailAnswer(bankAccountDetails)))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                         => fail()
    }

  def getGlobalErrors(doc: Document) = doc.getElementsByClass("govuk-error-summary__list").select("li")

  "Bank Account Controller" when {

    "Business Bank Account" should {

      val businessBankAccount = BankAccountDetails(
        accountName = AccountName("Some Account"),
        isBusinessAccount = Some(true),
        sortCode = SortCode("123456"),
        accountNumber = AccountNumber("12345678")
      )

      "Let users through when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid = Yes and accountExists = Yes" in {
        val businessResponse   =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val answers            = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val updatedBankAccount = businessBankAccount.copy(accountNumber = AccountNumber("87654321"))
        val updatedSession     = updateSession(session, updatedBankAccount)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
          mockBusinessReputation(Right(businessResponse))
        }
        val form               = BankAccountController.enterBankDetailsForm.fill(updatedBankAccount).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result             = controller.enterBankAccountDetailsSubmit(request)

        checkIsRedirect(result, SupportingEvidenceController.uploadSupportingEvidence())
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = (Indeterminate or Error or No) and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in {
        val answers         = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _) = sessionWithClaimState(Some(answers))
        val form            = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request         = FakeRequest().withFormUrlEncodedBody(form: _*)

        val accountNumberWithSortCodeIsValidCases = Seq(No, Indeterminate, ReputationResponse.Error)
        val accountExistsCases                    = Seq(Some(No), Some(Indeterminate), Some(ReputationResponse.Error), None)

        accountNumberWithSortCodeIsValidCases.foreach(accountNumberWithSortCodeIsValid =>
          accountExistsCases.foreach { accountExists =>
            val businessResponse =
              CommonBarsResponse(
                accountNumberWithSortCodeIsValid = accountNumberWithSortCodeIsValid,
                accountExists = accountExists,
                otherError = None
              )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockBusinessReputation(Right(businessResponse))
            }
            val result           = controller.enterBankAccountDetailsSubmit(request)
            val doc              = Jsoup.parse(contentAsString(result))
            val error            = getGlobalErrors(doc).text()
            error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed")
            status(result) shouldBe BAD_REQUEST
          }
        )
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = Yes and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in {
        val answers            = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val form               = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountExistsCases = Seq(Some(No), Some(Indeterminate), Some(ReputationResponse.Error), None)

        accountExistsCases.foreach { accountExists =>
          val businessResponse =
            CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = accountExists, otherError = None)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockBusinessReputation(Right(businessResponse))
          }
          val result           = controller.enterBankAccountDetailsSubmit(request)
          val doc              = Jsoup.parse(contentAsString(result))
          val error            = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist")
          status(result) shouldBe BAD_REQUEST
        }

      }

      "Fail when the Bank Account Number is invalid" in {
        val errorResponse    =
          ReputationErrorResponse(code = "INVALID_ACCOUNT_NUMBER", desc = "123456: invalid account number")
        val businessResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Yes,
          accountExists = Some(Yes),
          otherError = Some(errorResponse)
        )
        val answers          = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockBusinessReputation(Right(businessResponse))
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(businessBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.INVALID_ACCOUNT_NUMBER")
        status(result) shouldBe BAD_REQUEST
      }

    }

    "Personal Bank Account" should {

      val personalBankAccount = BankAccountDetails(
        accountName = AccountName("Some Account"),
        isBusinessAccount = None,
        sortCode = SortCode("123456"),
        accountNumber = AccountNumber("12345678")
      )

      "Let users through when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid" in {
        val personalResponse   =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val answers            = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val updatedBankAccount = personalBankAccount.copy(accountNumber = AccountNumber("87654321"))
        val updatedSession     = updateSession(session, updatedBankAccount)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
          mockPersonalReputation(Right(personalResponse))
        }
        val form               = BankAccountController.enterBankDetailsForm.fill(updatedBankAccount).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result             = controller.enterBankAccountDetailsSubmit(request)

        checkIsRedirect(result, SupportingEvidenceController.uploadSupportingEvidence())
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = (Indeterminate or Error or No) and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in {
        val answers                               = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)                       = sessionWithClaimState(Some(answers))
        val form                                  = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request                               = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountNumberWithSortCodeIsValidCases = Seq(No, Indeterminate, ReputationResponse.Error)
        val accountExistsCases                    = Seq(Some(No), Some(Indeterminate), Some(ReputationResponse.Error), None)

        accountNumberWithSortCodeIsValidCases.foreach(accountNumberWithSortCodeIsValid =>
          accountExistsCases.foreach { accountExists =>
            val personalResponse =
              CommonBarsResponse(
                accountNumberWithSortCodeIsValid = accountNumberWithSortCodeIsValid,
                accountExists = accountExists,
                otherError = None
              )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockPersonalReputation(Right(personalResponse))
            }
            val result           = controller.enterBankAccountDetailsSubmit(request)
            val doc              = Jsoup.parse(contentAsString(result))
            val error            = getGlobalErrors(doc).text()
            error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed")
            status(result) shouldBe BAD_REQUEST
          }
        )
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = Yes and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in {
        val answers            = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val form               = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountExistsCases = Seq(Some(No), Some(Indeterminate), Some(ReputationResponse.Error), None)

        accountExistsCases.foreach { accountExists =>
          val personalResponse =
            CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = accountExists, otherError = None)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockPersonalReputation(Right(personalResponse))
          }
          val result           = controller.enterBankAccountDetailsSubmit(request)
          val doc              = Jsoup.parse(contentAsString(result))
          val error            = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist")
          status(result) shouldBe BAD_REQUEST
        }

      }

      "Fail when the Sort Code is invalid" in {
        val errorResponse    = ReputationErrorResponse(code = "INVALID_SORTCODE", desc = "1234: invalid sortcode")
        val personalResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Yes,
          accountExists = Some(Yes),
          otherError = Some(errorResponse)
        )
        val answers          = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)  = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockPersonalReputation(Right(personalResponse))
        }
        val form             = BankAccountController.enterBankDetailsForm.fill(personalBankAccount).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.INVALID_SORTCODE")
        status(result) shouldBe BAD_REQUEST
      }

    }

  }

  "Form Validation" must {
    val form = BankAccountController.enterBankDetailsForm
    val accountName = "enter-bank-details.account-name"
    val isBusiness = "enter-bank-details.is-business-account"
    val sortCode = "enter-bank-details.sort-code"
    val accountNumber = "enter-bank-details.account-number"

    val goodData = Map(
      accountName -> "Barkhan Seer",
      isBusiness -> "false",
      sortCode -> "123456",
      accountNumber -> "12345678"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "accountName" should {
      "Accept longest possible names" in {
        val errors = form.bind(goodData.updated(accountName, alphaNumGen(40))).errors
        errors shouldBe Nil
      }
      "Reject names too long" in {
        val errors = form.bind(goodData.updated(accountName, alphaNumGen(41))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }


    "sortCode" should {
      "Accept longest possible sortCode" in {
        val errors = form.bind(goodData.updated(sortCode, numStringGen(6))).errors
        errors shouldBe Nil
      }
      "Reject sortCode too short" in {
        val errors = form.bind(goodData.updated(sortCode, numStringGen(5))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.minLength")
      }
      "Reject sortCode too long" in {
        val errors = form.bind(goodData.updated(sortCode, numStringGen(7))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }


    "accountNumber" should {
      "Accept shortest possible accountNumber" in {
        val errors = form.bind(goodData.updated(accountNumber, numStringGen(6))).errors
        errors shouldBe Nil
      }
      "Accept longest possible accountNumber" in {
        val errors = form.bind(goodData.updated(accountNumber, numStringGen(8))).errors
        errors shouldBe Nil
      }
      "Reject accountNumber too short" in {
        val errors = form.bind(goodData.updated(accountNumber, numStringGen(5))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.minLength")
      }
      "Reject accountNumber too long" in {
        val errors = form.bind(goodData.updated(accountNumber, numStringGen(9))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }
  }
}
