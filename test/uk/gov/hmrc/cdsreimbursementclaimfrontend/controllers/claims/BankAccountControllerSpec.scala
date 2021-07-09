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
import org.jsoup.select.Elements
import org.scalamock.scalatest.MockFactory
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.{AccountNumber, BankAccountDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.routes.SupportingEvidenceController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.CompleteBankAccountDetailAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidenceAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.{BarsBusinessAssessRequest, BarsPersonalAssessRequest}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.{Indeterminate, No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.{CommonBarsResponse, ReputationErrorResponse, ReputationResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeBankDetails, DeclarantBankDetails, DisplayDeclaration, DisplayResponseDetail, MaskedBankDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetailsAnswer, Error, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

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
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(claimService)
    )

  private def sessionWithClaimState(
    maybeBankAccountDetails: Option[BankAccountDetailsAnswer],
    supportingEvidences: Option[SupportingEvidenceAnswer] = None
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {

    val draftC285Claim =
      DraftC285Claim.newDraftC285Claim.copy(
        bankAccountDetailsAnswer = maybeBankAccountDetails,
        supportingEvidenceAnswer = supportingEvidences
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

  private def sessionWithMaskedBankDetails(
    maybeMaskedBankDetails: Option[MaskedBankDetails]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val displayResponseDetail = sample[DisplayResponseDetail].copy(maskedBankDetails = maybeMaskedBankDetails)
    val draftC285Claim        =
      DraftC285Claim.newDraftC285Claim.copy(displayDeclaration = Some(DisplayDeclaration(displayResponseDetail)))
    val ggCredId              = sample[GGCredId]
    val signedInUserDetails   = sample[SignedInUserDetails]
    val journey               = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
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

  def getGlobalErrors(doc: Document): Elements = doc.getElementsByClass("govuk-error-summary__list").select("li")

  "Bank Account Controller" when {

    "Check Bank Account Details" should {

      "Redirect when MaskedBankDetails is empty" in {
        val maskedBankDetails = MaskedBankDetails(None, None)
        val (session, _, _)   = sessionWithMaskedBankDetails(Some(maskedBankDetails))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request           = FakeRequest()
        val result            = controller.checkBankAccountDetails()(request)
        checkIsRedirect(result, routes.BankAccountController.enterBankAccountDetails())
      }

      "Redirect when MaskedBankDetails is None" in {
        val (session, _, _) = sessionWithMaskedBankDetails(None)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request         = FakeRequest()
        val result          = controller.checkBankAccountDetails()(request)
        checkIsRedirect(result, routes.BankAccountController.enterBankAccountDetails())
      }

      "Ok when MaskedBankDetails has consigneeBankDetails" in {
        val consigneeDetails  = sample[ConsigneeBankDetails]
        val maskedBankDetails = MaskedBankDetails(Some(consigneeDetails), None)
        val (session, _, _)   = sessionWithMaskedBankDetails(Some(maskedBankDetails))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request           = FakeRequest()
        val result            = controller.checkBankAccountDetails()(request)
        status(result) shouldBe OK
        //checkPageIsDisplayed(result, messageFromMessageKey("bank-details.title"))
      }

      "Ok when MaskedBankDetails has declarantBankDetails" in {
        val declarantBankDetails = sample[DeclarantBankDetails]
        val maskedBankDetails    = MaskedBankDetails(None, Some(declarantBankDetails))
        val (session, _, _)      = sessionWithMaskedBankDetails(Some(maskedBankDetails))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request              = FakeRequest()
        val result               = controller.checkBankAccountDetails()(request)
        status(result) shouldBe OK
        //checkPageIsDisplayed(result, messageFromMessageKey("bank-details.title"))
      }
    }

    "Business Bank Account" should {

      val businessBankAccount = sample[BankAccountDetails].copy(isBusinessAccount = Some(true))

      "Let users to upload supporting evidence when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid = Yes and accountExists = Yes" in {
        val businessResponse   =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val answers            = CompleteBankAccountDetailAnswer(businessBankAccount)
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val updatedBankAccount = businessBankAccount.copy(accountNumber = sample[AccountNumber])
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

      "Let skip upload supporting evidence page once users already uploaded evidences" in {
        val businessResponse         =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val bankDetailsAnswer        = CompleteBankAccountDetailAnswer(businessBankAccount)
        val supportingEvidenceAnswer = sample[SupportingEvidenceAnswer]
        val (session, _, _)          = sessionWithClaimState(Some(bankDetailsAnswer), Some(supportingEvidenceAnswer))
        val updatedBankAccount       = businessBankAccount.copy(accountNumber = sample[AccountNumber])
        val updatedSession           = updateSession(session, updatedBankAccount)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
          mockBusinessReputation(Right(businessResponse))
        }
        val form                     = BankAccountController.enterBankDetailsForm.fill(updatedBankAccount).data.toSeq
        val request                  = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result                   = controller.enterBankAccountDetailsSubmit(request)

        checkIsRedirect(result, SupportingEvidenceController.checkYourAnswers())
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

      val personalBankAccount = sample[BankAccountDetails].copy(isBusinessAccount = None)

      "Let users to upload supporting evidence when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid" in {
        val personalResponse   =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val answers            = CompleteBankAccountDetailAnswer(personalBankAccount)
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val updatedBankAccount = personalBankAccount.copy(accountNumber = sample[AccountNumber])
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

      "Let skip supporting evidence upload page once users already has uploaded evidences" in {
        val personalResponse =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)

        val bankAccountAnswer = CompleteBankAccountDetailAnswer(personalBankAccount)
        val evidenceAnswer    = sample[SupportingEvidenceAnswer]

        val (session, _, _)    = sessionWithClaimState(Some(bankAccountAnswer), Some(evidenceAnswer))
        val updatedBankAccount = personalBankAccount.copy(accountNumber = sample[AccountNumber])
        val updatedSession     = updateSession(session, updatedBankAccount)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
          mockPersonalReputation(Right(personalResponse))
        }

        val form    = BankAccountController.enterBankDetailsForm.fill(updatedBankAccount).data.toSeq
        val request = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result  = controller.enterBankAccountDetailsSubmit(request)

        checkIsRedirect(result, SupportingEvidenceController.checkYourAnswers())
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
    val form          = BankAccountController.enterBankDetailsForm
    val accountName   = "enter-bank-details.account-name"
    val isBusiness    = "enter-bank-details.is-business-account"
    val sortCode      = "enter-bank-details.sort-code"
    val accountNumber = "enter-bank-details.account-number"

    val goodData = Map(
      accountName   -> "Barkhan Seer",
      isBusiness    -> "false",
      sortCode      -> "123456",
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
      "Accept sort code with spaces" in {
        val errors = form.bind(goodData.updated(sortCode, "10 20 30")).errors
        errors shouldBe Nil
      }
      "Accept sort code with dashes" in {
        val errors = form.bind(goodData.updated(sortCode, "10-20-30")).errors
        errors shouldBe Nil
      }
      "Accept sort code with a mixture of spaces and dashes" in {
        val errors = form.bind(goodData.updated(sortCode, "10 -20--30")).errors
        errors shouldBe Nil
      }
      "Reject sortCode too short" in {
        val errors = form.bind(goodData.updated(sortCode, numStringGen(5))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid")
      }
      "Reject sortCode too long" in {
        val errors = form.bind(goodData.updated(sortCode, numStringGen(7))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid")
      }
    }

    "accountNumber" should {
      "Accept shortest possible (6 digits) accountNumber and pad it" in {
        val genAccountNumber = numStringGen(6)
        val bandkAccountForm = form.bind(goodData.updated(accountNumber, genAccountNumber))
        bandkAccountForm.errors                                           shouldBe Nil
        bandkAccountForm.value.map(_.accountNumber.value).getOrElse(fail) shouldBe "00" + genAccountNumber
      }
      "Accept 7 digits accountNumber and pad it" in {
        val genAccountNumber = numStringGen(7)
        val bandkAccountForm = form.bind(goodData.updated(accountNumber, genAccountNumber))
        bandkAccountForm.errors                                           shouldBe Nil
        bandkAccountForm.value.map(_.accountNumber.value).getOrElse(fail) shouldBe "0" + genAccountNumber
      }
      "Accept longest possible (8 digits) accountNumber" in {
        val genAccountNumber = numStringGen(8)
        val bandkAccountForm = form.bind(goodData.updated(accountNumber, genAccountNumber))
        bandkAccountForm.errors                                           shouldBe Nil
        bandkAccountForm.value.map(_.accountNumber.value).getOrElse(fail) shouldBe genAccountNumber
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
