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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.implicits._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsBusinessAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsPersonalAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Indeterminate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.CommonBarsResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationErrorResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.BadGatewayException
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BankAccountControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks
    with MockFactory {

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

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
    maybeBankAccountDetails: Option[BankAccountDetails],
    bankAccountType: Option[BankAccountType],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer],
    supportingEvidences: Option[SupportingEvidencesAnswer] = None
  ): (SessionData, FillingOutClaim, DraftClaim) = {

    val draftC285Claim =
      DraftClaim.blank.copy(
        bankAccountDetailsAnswer = maybeBankAccountDetails,
        bankAccountTypeAnswer = bankAccountType,
        supportingEvidencesAnswer = supportingEvidences,
        typeOfClaim = maybeTypeOfClaim,
        movementReferenceNumber = Some(sample[MRN])
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
    maybeMaskedBankDetails: Option[BankDetails],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val displayResponseDetail = sample[DisplayResponseDetail].copy(maskedBankDetails = maybeMaskedBankDetails)
    val draftC285Claim        =
      DraftClaim.blank.copy(
        displayDeclaration = Some(DisplayDeclaration(displayResponseDetail)),
        typeOfClaim = maybeTypeOfClaim,
        movementReferenceNumber = Some(sample[MRN])
      )
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
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      =
          draftClaim.copy(bankAccountDetailsAnswer = Some(bankAccountDetails))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }

  def getGlobalErrors(doc: Document): Elements = doc.getElementsByClass("govuk-error-summary__list").select("li")

  def getAccountNameValue(doc: Document): String =
    doc.select("input[name='enter-bank-details.account-name']").first().attr("value")

  "Bank Account Controller" when {

    "Check Bank Account Details" should {

      "Redirect when MaskedBankDetails is empty" in forAll(journeys) { journey =>
        val maskedBankDetails = BankDetails(None, None)
        val (session, _, _)   =
          sessionWithMaskedBankDetails(Some(maskedBankDetails), toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)

        }

        val request = FakeRequest()
        val result  = controller.checkBankAccountDetails(journey)(request)

        checkIsRedirect(result, routes.SelectBankAccountTypeController.selectBankAccountType(journey))

      }

      "Redirect when MaskedBankDetails is None" in forAll(journeys) { journey =>
        val (session, _, _) = sessionWithMaskedBankDetails(None, toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.checkBankAccountDetails(journey)(request)
        checkIsRedirect(result, routes.SelectBankAccountTypeController.selectBankAccountType(journey))
      }

      "Ok when MaskedBankDetails has consigneeBankDetails" in forAll(journeys) { journey =>
        val consigneeDetails  = sample[BankAccountDetails]
        val maskedBankDetails = BankDetails(Some(consigneeDetails), None)
        val (session, _, _)   =
          sessionWithMaskedBankDetails(Some(maskedBankDetails), toTypeOfClaim(journey).some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request           = FakeRequest()
        val result            = controller.checkBankAccountDetails(journey)(request)
        status(result) shouldBe OK
      }

      "Ok when MaskedBankDetails has declarantBankDetails" in forAll(journeys) { journey =>
        val declarantBankDetails = sample[BankAccountDetails]
        val maskedBankDetails    = BankDetails(None, Some(declarantBankDetails))
        val (session, _, _)      =
          sessionWithMaskedBankDetails(Some(maskedBankDetails), toTypeOfClaim(journey).some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request              = FakeRequest()
        val result               = controller.checkBankAccountDetails(journey)(request)
        status(result) shouldBe OK

      }
    }

    "Business Bank Account" should {

      val Business        = sample[BankAccountDetails]
      val bankAccountType = BankAccountType.Business

      "Let users check their bank details when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid = Yes and accountExists = Yes" in forAll(
        journeys
      ) { journey =>
        val businessResponse   =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val answers            = Business
        val (session, _, _)    =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val updatedBankAccount = Business.copy(accountNumber = sample[AccountNumber])
        val updatedSession     = updateSession(session, updatedBankAccount)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
          mockBusinessReputation(Right(businessResponse))
        }
        val form               = enterBankDetailsForm.fill(updatedBankAccount).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result             = controller.enterBankAccountDetailsSubmit(journey)(request)

        checkIsRedirect(result, routes.BankAccountController.checkBankAccountDetails(journey))
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = No and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in forAll(
        journeys
      ) { journey =>
        val answers         = Business
        val (session, _, _) =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form            = enterBankDetailsForm.fill(Business).data.toSeq
        val request         = FakeRequest().withFormUrlEncodedBody(form: _*)

        val accountExistsCases = Seq(Some(No), Some(Indeterminate), Some(ReputationResponse.Error), None)

        accountExistsCases.foreach { accountExists =>
          val businessResponse =
            CommonBarsResponse(
              accountNumberWithSortCodeIsValid = No,
              accountExists = accountExists,
              otherError = None
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockBusinessReputation(Right(businessResponse))
          }
          val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
          val doc              = Jsoup.parse(contentAsString(result))
          getAccountNameValue(doc) shouldBe ""
          val error = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-no")
          status(result) shouldBe BAD_REQUEST
        }
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = (Indeterminate or Error) and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in forAll(
        journeys
      ) { journey =>
        val answers         = Business
        val (session, _, _) =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form            = enterBankDetailsForm.fill(Business).data.toSeq
        val request         = FakeRequest().withFormUrlEncodedBody(form: _*)

        val accountNumberWithSortCodeIsValidCases = Seq(Indeterminate, ReputationResponse.Error)
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
            val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
            val doc              = Jsoup.parse(contentAsString(result))
            getAccountNameValue(doc) shouldBe ""
            val error = getGlobalErrors(doc).text()
            error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed")
            status(result) shouldBe BAD_REQUEST
          }
        )
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = Yes and accountExists = Some(Error)" in forAll(
        journeys
      ) { journey =>
        val answers         = Business
        val (session, _, _) =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form            = enterBankDetailsForm.fill(Business).data.toSeq
        val request         = FakeRequest().withFormUrlEncodedBody(form: _*)

        val businessResponse =
          CommonBarsResponse(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = Some(ReputationResponse.Error),
            otherError = None
          )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockBusinessReputation(Right(businessResponse))
        }
        val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
        val doc              = Jsoup.parse(contentAsString(result))
        getAccountNameValue(doc) shouldBe Business.accountName.value
        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.account-exists-error")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = Yes and accountExists = (Some(Indeterminate) or Some(No) or None)" in forAll(
        journeys
      ) { journey =>
        val answers            = Business
        val (session, _, _)    =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form               = enterBankDetailsForm.fill(Business).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountExistsCases = Seq(Some(No), Some(Indeterminate), None)

        accountExistsCases.foreach { accountExists =>
          val businessResponse =
            CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = accountExists, otherError = None)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockBusinessReputation(Right(businessResponse))
          }
          val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
          val doc              = Jsoup.parse(contentAsString(result))
          getAccountNameValue(doc) shouldBe ""
          val error = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist")
          status(result) shouldBe BAD_REQUEST
        }

      }

      "Fail when the Bank Account Number is invalid" in forAll(journeys) { journey =>
        val errorResponse    =
          ReputationErrorResponse(code = "INVALID_ACCOUNT_NUMBER", desc = "123456: invalid account number")
        val businessResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Yes,
          accountExists = Some(Yes),
          otherError = Some(errorResponse)
        )
        val answers          = Business
        val (session, _, _)  =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockBusinessReputation(Right(businessResponse))
        }
        val form             = enterBankDetailsForm.fill(Business).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.INVALID_ACCOUNT_NUMBER")
        status(result) shouldBe BAD_REQUEST
      }

      "Redirect to error page if the bank reputation service is down" in forAll(journeys) { journey =>
        val errorResponse   = Error(
          new BadGatewayException(
            "POST of 'http://localhost:7502/personal/v3/assess' failed. Caused by: 'Connection refused: localhost/0:0:0:0:0:0:0:1:7502'"
          )
        )
        val answers         = Business
        val (session, _, _) =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockBusinessReputation(Left(errorResponse))
        }

        val form    = enterBankDetailsForm.fill(Business).data.toSeq
        val request = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result  = controller.enterBankAccountDetailsSubmit(journey)(request)

        checkIsRedirect(result, routes.ServiceUnavailableController.unavailable(journey))
      }
    }

    "Personal Bank Account" should {

      val Personal        = sample[BankAccountDetails]
      val bankAccountType = BankAccountType.Personal

      "Let users check their bank details when the Bank Account Validation succeeds with accountNumberWithSortCodeIsValid" in forAll(
        journeys
      ) { journey =>
        val personalResponse   =
          CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), otherError = None)
        val answers            = Personal
        val (session, _, _)    =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val updatedBankAccount = Personal.copy(accountNumber = sample[AccountNumber])
        val updatedSession     = updateSession(session, updatedBankAccount)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
          mockPersonalReputation(Right(personalResponse))
        }

        val form    = enterBankDetailsForm.fill(updatedBankAccount).data.toSeq
        val request = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result  = controller.enterBankAccountDetailsSubmit(journey)(request)

        checkIsRedirect(result, routes.BankAccountController.checkBankAccountDetails(journey))
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = No and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in forAll(
        journeys
      ) { journey =>
        val answers            = Personal
        val (session, _, _)    =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form               = enterBankDetailsForm.fill(Personal).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountExistsCases = Seq(Some(No), Some(Indeterminate), Some(ReputationResponse.Error), None)

        accountExistsCases.foreach { accountExists =>
          val personalResponse =
            CommonBarsResponse(
              accountNumberWithSortCodeIsValid = No,
              accountExists = accountExists,
              otherError = None
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockPersonalReputation(Right(personalResponse))
          }
          val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
          val doc              = Jsoup.parse(contentAsString(result))
          getAccountNameValue(doc) shouldBe ""
          val error = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-no")
          status(result) shouldBe BAD_REQUEST
        }
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = (Indeterminate or Error) and accountExists = (Some(Indeterminate) or Some(Error) or Some(No) or None)" in forAll(
        journeys
      ) { journey =>
        val answers                               = Personal
        val (session, _, _)                       =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form                                  = enterBankDetailsForm.fill(Personal).data.toSeq
        val request                               = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountNumberWithSortCodeIsValidCases = Seq(Indeterminate, ReputationResponse.Error)
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
            val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
            val doc              = Jsoup.parse(contentAsString(result))
            getAccountNameValue(doc) shouldBe ""
            val error = getGlobalErrors(doc).text()
            error          shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed")
            status(result) shouldBe BAD_REQUEST
          }
        )
      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = Yes and accountExists = Some(Error)" in forAll(
        journeys
      ) { journey =>
        val answers         = Personal
        val (session, _, _) =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form            = enterBankDetailsForm.fill(Personal).data.toSeq
        val request         = FakeRequest().withFormUrlEncodedBody(form: _*)

        val personalResponse =
          CommonBarsResponse(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = Some(ReputationResponse.Error),
            otherError = None
          )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockPersonalReputation(Right(personalResponse))
        }
        val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
        val doc              = Jsoup.parse(contentAsString(result))
        getAccountNameValue(doc) shouldBe Personal.accountName.value
        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.account-exists-error")
        status(result) shouldBe BAD_REQUEST

      }

      "Fail when the Bank Account Validation fails with accountNumberWithSortCodeIsValid = Yes and accountExists = (Some(Indeterminate) or Some(No) or None)" in forAll(
        journeys
      ) { journey =>
        val answers            = Personal
        val (session, _, _)    =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        val form               = enterBankDetailsForm.fill(Personal).data.toSeq
        val request            = FakeRequest().withFormUrlEncodedBody(form: _*)
        val accountExistsCases = Seq(Some(No), Some(Indeterminate), None)

        accountExistsCases.foreach { accountExists =>
          val personalResponse =
            CommonBarsResponse(accountNumberWithSortCodeIsValid = Yes, accountExists = accountExists, otherError = None)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockPersonalReputation(Right(personalResponse))
          }
          val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
          val doc              = Jsoup.parse(contentAsString(result))
          getAccountNameValue(doc) shouldBe ""
          val error = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist")
          status(result) shouldBe BAD_REQUEST
        }

      }

      "Fail when the Sort Code is invalid" in forAll(journeys) { journey =>
        val errorResponse    = ReputationErrorResponse(code = "INVALID_SORTCODE", desc = "1234: invalid sortcode")
        val personalResponse = CommonBarsResponse(
          accountNumberWithSortCodeIsValid = Yes,
          accountExists = Some(Yes),
          otherError = Some(errorResponse)
        )
        val answers          = Personal
        val (session, _, _)  =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockPersonalReputation(Right(personalResponse))
        }
        val form             = enterBankDetailsForm.fill(Personal).data.toSeq
        val request          = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result           = controller.enterBankAccountDetailsSubmit(journey)(request)
        val doc              = Jsoup.parse(contentAsString(result))

        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-bank-details.error.INVALID_SORTCODE")
        status(result) shouldBe BAD_REQUEST
      }

      "Redirect to error page if the bank reputation service is down" in forAll(journeys) { journey =>
        val errorResponse   = Error(
          new BadGatewayException(
            "POST of 'http://localhost:7502/personal/v3/assess' failed. Caused by: 'Connection refused: localhost/0:0:0:0:0:0:0:1:7502'"
          )
        )
        val answers         = Personal
        val (session, _, _) =
          sessionWithClaimState(Some(answers), Some(bankAccountType), toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockPersonalReputation(Left(errorResponse))
        }

        val form    = enterBankDetailsForm.fill(Personal).data.toSeq
        val request = FakeRequest().withFormUrlEncodedBody(form: _*)
        val result  = controller.enterBankAccountDetailsSubmit(journey)(request)

        checkIsRedirect(result, routes.ServiceUnavailableController.unavailable(journey))
      }

    }
  }

  "Form Validation" must {
    val form          = enterBankDetailsForm
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
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.length")
      }
      "Reject accountNumber too long" in {
        val errors = form.bind(goodData.updated(accountNumber, numStringGen(9))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.length")
      }
    }
  }
}
