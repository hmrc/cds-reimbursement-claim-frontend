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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import cats.data.EitherT
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.arbitraryBankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.genReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen.genBankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaNumGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.numStringGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterBankAccountDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockClaimService = mock[ClaimService]

  def mockBusinessReputation(
    bankAccountDetails: BankAccountDetails,
    response: Either[Error, BankAccountReputation]
  ) =
    (mockClaimService
      .getBusinessAccountReputation(_: BankAccountDetails)(_: HeaderCarrier))
      .expects(bankAccountDetails, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  def mockPersonalReputation(
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String],
    response: Either[Error, BankAccountReputation]
  ) =
    (mockClaimService
      .getPersonalAccountReputation(_: BankAccountDetails, _: Option[String])(_: HeaderCarrier))
      .expects(bankAccountDetails, postCode, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  val controller: EnterBankAccountDetailsController = instanceOf[EnterBankAccountDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit val request: Request[_] = FakeRequest()

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "enter-bank-details"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsMultipleJourney = Some(RejectedGoodsMultipleJourney.empty(exampleEori))
  )

  "Enter Bank Account Details Controller" must {

    "not find the page if rejected goods feature is disabled" in {
      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      featureSwitch.disable(Feature.RejectedGoods)

      status(performAction()) shouldBe NOT_FOUND
    }

    "display the page" when {
      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => selectedInput(doc) shouldBe empty
        )
      }

      "the user has answered this question before" in forAll(completeJourneyNotCMAEligibleGen) { journey =>
        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedInputBox(doc, "enter-bank-details.account-name")   shouldBe Some("")
            selectedInputBox(doc, "enter-bank-details.sort-code")      shouldBe Some("")
            selectedInputBox(doc, "enter-bank-details.account-number") shouldBe Some("")
          }
        )

      }

    }

    "validate bank account details" when {
      "redirect to choose bank account type page if no bank account type present in session" in forAll(
        genBankAccountDetails
      ) { bankDetails =>
        val journey = RejectedGoodsMultipleJourney.empty(exampleEori)
        checkIsRedirect(
          controller.validateBankAccountDetails(journey, bankDetails, None).map(_._2),
          routes.ChooseBankAccountTypeController.show()
        )
      }

      "personal account" must {
        val journey = RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitBankAccountType(BankAccountType.Personal)
          .getOrFail

        "redirect to the check bank accounts page if a personal account that exists with a valid sort code is specified" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode)
        ) { (bankDetails, postCode) =>
          val personalResponse =
            bankaccountreputation.BankAccountReputation(
              accountNumberWithSortCodeIsValid = Yes,
              accountExists = Some(Yes),
              otherError = None
            )

          inSequence(
            mockPersonalReputation(bankDetails, postCode, Right(personalResponse))
          )

          checkIsRedirect(
            controller.validateBankAccountDetails(journey, bankDetails, postCode).map(_._2),
            routes.CheckBankDetailsController.show()
          )
        }

        "show the bank account details page if there is an error response" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          arbitraryBankAccountReputation.arbitrary
        ) { (bankAccountDetails, postCode, genericResponse) =>
          val expectedResponse = genericResponse.copy(
            otherError = Some(ReputationErrorResponse("account-does-not-exist", "error"))
          )

          inSequence(
            mockPersonalReputation(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the account exists field is error" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode)
        ) { (bankAccountDetails, postCode) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = Some(ReputationResponse.Error),
            otherError = None
          )

          inSequence(
            mockPersonalReputation(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.account-exists-error"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the sort code is invalid" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          genReputationResponse
        ) { (bankAccountDetails, postCode, possibleResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = ReputationResponse.No,
            accountExists = Some(possibleResponse),
            otherError = None
          )

          inSequence(
            mockPersonalReputation(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-no"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the sort code is anything other than yes or no" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Inapplicable, Indeterminate, ReputationResponse.Error),
          genReputationResponse
        ) { (bankAccountDetails, postCode, sortCodeResponse, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = sortCodeResponse,
            accountExists = Some(accountResponse),
            otherError = None
          )

          inSequence(
            mockPersonalReputation(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the account number is not valid" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Some(Inapplicable), Some(Indeterminate), Some(No), None)
        ) { (bankAccountDetails, postCode, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = accountResponse,
            otherError = None
          )

          inSequence(
            mockPersonalReputation(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist"),
            BAD_REQUEST
          )
        }

      }

      "business account" must {
        val journey = RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitBankAccountType(BankAccountType.Business)
          .getOrFail

        "redirect to the check bank accounts page if a business account that exists with a valid sort code is specified" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode)
        ) { (bankDetails, postCode) =>
          val personalResponse =
            bankaccountreputation.BankAccountReputation(
              accountNumberWithSortCodeIsValid = Yes,
              accountExists = Some(Yes),
              otherError = None
            )

          inSequence(
            mockBusinessReputation(bankDetails, Right(personalResponse))
          )

          checkIsRedirect(
            controller.validateBankAccountDetails(journey, bankDetails, postCode).map(_._2),
            routes.CheckBankDetailsController.show()
          )
        }

        "show the bank account details page if there is an error response" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          arbitraryBankAccountReputation.arbitrary
        ) { (bankAccountDetails, postCode, genericResponse) =>
          val expectedResponse = genericResponse.copy(
            otherError = Some(ReputationErrorResponse("account-does-not-exist", "error"))
          )

          inSequence(
            mockBusinessReputation(bankAccountDetails, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the account exists field is error" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode)
        ) { (bankAccountDetails, postCode) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = Some(ReputationResponse.Error),
            otherError = None
          )

          inSequence(
            mockBusinessReputation(bankAccountDetails, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.account-exists-error"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the sort code is invalid" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          genReputationResponse
        ) { (bankAccountDetails, postCode, possibleResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = ReputationResponse.No,
            accountExists = Some(possibleResponse),
            otherError = None
          )

          inSequence(
            mockBusinessReputation(bankAccountDetails, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-no"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the sort code is anything other than yes or no" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Inapplicable, Indeterminate, ReputationResponse.Error),
          genReputationResponse
        ) { (bankAccountDetails, postCode, sortCodeResponse, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = sortCodeResponse,
            accountExists = Some(accountResponse),
            otherError = None
          )

          inSequence(
            mockBusinessReputation(bankAccountDetails, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.moc-check-failed"),
            BAD_REQUEST
          )
        }

        "show the bank account details page if the account number is not valid" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Some(Inapplicable), Some(Indeterminate), Some(No), None)
        ) { (bankAccountDetails, postCode, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = accountResponse,
            otherError = None
          )

          inSequence(
            mockBusinessReputation(bankAccountDetails, Right(expectedResponse))
          )

          checkPageIsDisplayed(
            controller.validateBankAccountDetails(journey, bankAccountDetails, postCode).map(_._2),
            messageFromMessageKey(s"$messagesKey.title"),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey("enter-bank-details.error.account-does-not-exist"),
            BAD_REQUEST
          )
        }
      }
    }

    "handle submit requests" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "the user enters details for the first time" in forAll(genBankAccountDetails) { bankDetails =>
        val initialJourney  =
          RejectedGoodsMultipleJourney.empty(exampleEori).submitBankAccountType(BankAccountType.Personal).getOrFail
        val requiredSession = session.copy(rejectedGoodsMultipleJourney = Some(initialJourney))

        val updatedJourney             = initialJourney.submitBankAccountDetails(bankDetails)
        val updatedSession             = session.copy(rejectedGoodsMultipleJourney = updatedJourney.toOption)
        val expectedSuccessfulResponse = bankaccountreputation.BankAccountReputation(
          accountNumberWithSortCodeIsValid = Yes,
          accountExists = Some(Yes),
          otherError = None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(requiredSession)
          mockPersonalReputation(bankDetails, None, Right(expectedSuccessfulResponse))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(
            s"${controller.formKey}.account-name"   -> bankDetails.accountName.value,
            s"${controller.formKey}.sort-code"      -> bankDetails.sortCode.value,
            s"${controller.formKey}.account-number" -> bankDetails.accountNumber.value
          ),
          routes.CheckBankDetailsController.show()
        )

      }

      "Redirect to bank account type page if not specified" in forAll(genBankAccountDetails) { bankDetails =>
        val initialJourney  = RejectedGoodsMultipleJourney.empty(exampleEori)
        val requiredSession = session.copy(rejectedGoodsMultipleJourney = Some(initialJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(requiredSession)
        }

        checkIsRedirect(
          performAction(
            s"${controller.formKey}.account-name"   -> bankDetails.accountName.value,
            s"${controller.formKey}.sort-code"      -> bankDetails.sortCode.value,
            s"${controller.formKey}.account-number" -> bankDetails.accountNumber.value
          ),
          routes.ChooseBankAccountTypeController.show()
        )
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

      "Reject an empty field" in {
        val errors = form.bind(goodData.updated(accountName, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
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
      "Reject an empty field" in {
        val errors = form.bind(goodData.updated(sortCode, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
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
      "Reject an empty field" in {
        val errors = form.bind(goodData.updated(accountNumber, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
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