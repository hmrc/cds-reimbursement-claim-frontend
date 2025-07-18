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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MockBankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.arbitraryBankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.genReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.genBankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaNumGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.numStringGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.http.BadRequestException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
class EnterBankAccountDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with MockBankAccountReputationService {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[BankAccountReputationService].toInstance(mockBankAccountReputationService)
    )

  val controller: EnterBankAccountDetailsController = instanceOf[EnterBankAccountDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit val request: Request[?] = FakeRequest()

  private val messagesKey: String = "enter-bank-account-details"

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  "Enter Bank Account Details Controller" must {

    "display the page" when {
      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedInputBox(doc, "enter-bank-account-details.account-name")   shouldBe Some("")
            selectedInputBox(doc, "enter-bank-account-details.sort-code")      shouldBe Some("")
            selectedInputBox(doc, "enter-bank-account-details.account-number") shouldBe Some("")
          }
        )
      }

      "the user has answered this question before" in forAll(completeJourneyGen) { journey =>
        val updatedSession = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedInputBox(doc, "enter-bank-account-details.account-name") shouldBe journey.answers.bankAccountDetails
              .map(_.accountName.value)
            selectedInputBox(doc, "enter-bank-account-details.sort-code")    shouldBe journey.answers.bankAccountDetails
              .map(_.sortCode.value)
            selectedInputBox(
              doc,
              "enter-bank-account-details.account-number"
            )                                                                shouldBe journey.answers.bankAccountDetails.map(_.accountNumber.value)
          }
        )
      }
    }

    "validate bank account details" when {
      val journey: OverpaymentsMultipleJourney = completeJourneyGen.sample.get.resetBankTypeAndDetails()

      def validatedResult(
        bankAccountDetails: BankAccountDetails,
        bankAccountType: Option[BankAccountType] = None,
        postCode: Option[String] = None
      ): Future[Result] =
        controller
          .validateBankAccountDetails(
            bankAccountType
              .map(ba => journey.submitBankAccountType(ba).getOrFail)
              .getOrElse(journey),
            bankAccountDetails,
            postCode
          )
          .map(_._2)

      "personal account" must {
        "redirect to the check answers page if a personal account that exists with a valid sort code is specified and all questions answered" in
          forAll(
            genBankAccountDetails,
            Gen.option(genPostcode),
            Gen.oneOf(Yes, Indeterminate)
          ) { (bankDetails, postCode, accountExists) =>
            val personalResponse =
              bankaccountreputation.BankAccountReputation(
                accountNumberWithSortCodeIsValid = Yes,
                accountExists = Some(accountExists),
                otherError = None,
                accountName = Some(bankDetails.accountName.value),
                nameMatches = Some(ReputationResponse.Partial)
              )

            inSequence(
              mockBankAccountReputationV2(bankDetails, postCode, Right(personalResponse))
            )

            checkIsRedirect(
              validatedResult(bankDetails, Some(BankAccountType.Personal), postCode),
              routes.CheckYourAnswersController.show
            )
          }

        "redirect to the check your answers page if a personal account that exists with a valid sort code is specified and Indeterminate and all questions answered" in
          forAll(
            genBankAccountDetails,
            Gen.option(genPostcode)
          ) { (bankDetails, postCode) =>
            val personalResponse =
              bankaccountreputation.BankAccountReputation(
                accountNumberWithSortCodeIsValid = Indeterminate,
                accountExists = Some(Yes),
                otherError = None,
                accountName = None,
                nameMatches = Some(ReputationResponse.Yes)
              )

            inSequence(
              mockBankAccountReputationV2(bankDetails, postCode, Right(personalResponse))
            )

            checkIsRedirect(
              validatedResult(bankDetails, Some(BankAccountType.Personal), postCode),
              routes.CheckYourAnswersController.show
            )
          }

        "show error on the the bank account details page if there is an error response" in
          forAll(
            genBankAccountDetails,
            Gen.option(genPostcode),
            arbitraryBankAccountReputation.arbitrary
          ) { (bankAccountDetails, postCode, genericResponse) =>
            val expectedResponse = genericResponse.copy(
              otherError = Some(ReputationErrorResponse("account-does-not-exist", "error")),
              accountExists = Some(ReputationResponse.No)
            )

            inSequence(
              mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
            )

            checkIsRedirect(
              validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
              routes.CheckBankDetailsController.showWarning
            )
          }

        "show error on the the bank account details page if the account exists field is error" in
          forAll(
            genBankAccountDetails,
            Gen.option(genPostcode)
          ) { (bankAccountDetails, postCode) =>
            val expectedResponse = bankaccountreputation.BankAccountReputation(
              accountNumberWithSortCodeIsValid = Yes,
              accountExists = Some(ReputationResponse.Error),
              otherError = None
            )

            inSequence(
              mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
            )

            checkIsRedirect(
              validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
              routes.CheckBankDetailsController.showWarning
            )
          }
        "show error on the the bank account details page if the sort code is invalid" in forAll(
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
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if Inapplicable" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.const(Inapplicable),
          genReputationResponse
        ) { (bankAccountDetails, postCode, sortCodeResponse, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = sortCodeResponse,
            accountExists = Some(accountResponse),
            otherError = None
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if ReputationResponse.Error" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.const(ReputationResponse.Error),
          genReputationResponse
        ) { (bankAccountDetails, postCode, sortCodeResponse, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = sortCodeResponse,
            accountExists = Some(accountResponse),
            otherError = None
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if the account number is not valid" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Some(Inapplicable), Some(No), None)
        ) { (bankAccountDetails, postCode, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = accountResponse,
            otherError = None
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }
      }

      "business account" must {
        "redirect to the check answers page if a business account that exists with a valid sort code is specified and all questions answered" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode)
        ) { (bankAccountDetails, postCode) =>
          val personalResponse =
            bankaccountreputation.BankAccountReputation(
              accountNumberWithSortCodeIsValid = Yes,
              accountExists = Some(Yes),
              otherError = None,
              accountName = None,
              nameMatches = Some(ReputationResponse.Yes)
            )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(personalResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Business), postCode),
            routes.CheckYourAnswersController.show
          )
        }

        "show error on the the bank account details page if there is an error response" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          arbitraryBankAccountReputation.arbitrary
        ) { (bankAccountDetails, postCode, genericResponse) =>
          val expectedResponse = genericResponse.copy(
            otherError = Some(ReputationErrorResponse("account-does-not-exist", "error")),
            accountExists = Some(ReputationResponse.No)
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Business), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if the account exists field is error" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode)
        ) { (bankAccountDetails, postCode) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = Some(ReputationResponse.Error),
            otherError = None
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Business), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if the sort code is invalid" in forAll(
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
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Business), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if the sort code is anything other than yes or no" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Inapplicable, ReputationResponse.Error),
          genReputationResponse
        ) { (bankAccountDetails, postCode, sortCodeResponse, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = sortCodeResponse,
            accountExists = Some(accountResponse),
            otherError = None
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Business), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }

        "show error on the the bank account details page if the account number is not valid" in forAll(
          genBankAccountDetails,
          Gen.option(genPostcode),
          Gen.oneOf(Some(Inapplicable), Some(No), None)
        ) { (bankAccountDetails, postCode, accountResponse) =>
          val expectedResponse = bankaccountreputation.BankAccountReputation(
            accountNumberWithSortCodeIsValid = Yes,
            accountExists = accountResponse,
            otherError = None
          )

          inSequence(
            mockBankAccountReputationV2(bankAccountDetails, postCode, Right(expectedResponse))
          )

          checkIsRedirect(
            validatedResult(bankAccountDetails, Some(BankAccountType.Business), postCode),
            routes.CheckBankDetailsController.showWarning
          )
        }
      }
    }

    "handle submit requests" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "the user enters details for the first time" in forAll(genBankAccountDetails) { bankDetails =>
        val initialJourney =
          OverpaymentsMultipleJourney
            .empty(exampleDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .flatMap(_.submitBankAccountType(BankAccountType.Personal))
            .getOrFail

        val requiredSession = SessionData(initialJourney)

        val updatedJourney             = initialJourney.submitBankAccountDetails(bankDetails)
        val updatedSession             = session.copy(overpaymentsMultipleJourney = updatedJourney.toOption)
        val expectedSuccessfulResponse = bankaccountreputation.BankAccountReputation(
          accountNumberWithSortCodeIsValid = Yes,
          accountExists = Some(Yes),
          otherError = None
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(requiredSession)
          mockBankAccountReputationV2(bankDetails, None, Right(expectedSuccessfulResponse))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(
            "enter-bank-account-details.account-name"   -> bankDetails.accountName.value,
            "enter-bank-account-details.sort-code"      -> bankDetails.sortCode.value,
            "enter-bank-account-details.account-number" -> bankDetails.accountNumber.value
          ),
          routes.ChooseFileTypeController.show
        )

      }

      "redirects to the service unavailable page when the BARS service returns a 400 BAD REQUEST" in forAll(
        genBankAccountDetails
      ) { bankDetails =>
        val initialJourney =
          OverpaymentsMultipleJourney
            .empty(exampleDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .flatMap(_.submitBankAccountType(BankAccountType.Personal))
            .getOrFail

        val requiredSession = SessionData(initialJourney)
        val boom            = new BadRequestException("Boom!")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(requiredSession)
          mockBankAccountReputationV2(
            bankDetails,
            None,
            Left(ServiceUnavailableError(boom.message, Some(boom)))
          )
        }

        checkIsRedirect(
          performAction(
            "enter-bank-account-details.account-name"   -> bankDetails.accountName.value,
            "enter-bank-account-details.sort-code"      -> bankDetails.sortCode.value,
            "enter-bank-account-details.account-number" -> bankDetails.accountNumber.value
          ),
          routes.CheckBankDetailsController.showWarning
        )
      }
    }
  }

  "Form Validation" must {
    val form          = enterBankDetailsForm
    val accountName   = "enter-bank-account-details.account-name"
    val isBusiness    = "enter-bank-account-details.is-business-account"
    val sortCode      = "enter-bank-account-details.sort-code"
    val accountNumber = "enter-bank-account-details.account-number"

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
        val errors = form.bind(goodData.updated(accountName, alphaNumGen(71))).errors
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
        bandkAccountForm.errors                                             shouldBe Nil
        bandkAccountForm.value.map(_.accountNumber.value).getOrElse(fail()) shouldBe "00" + genAccountNumber
      }
      "Accept 7 digits accountNumber and pad it" in {
        val genAccountNumber = numStringGen(7)
        val bandkAccountForm = form.bind(goodData.updated(accountNumber, genAccountNumber))
        bandkAccountForm.errors                                             shouldBe Nil
        bandkAccountForm.value.map(_.accountNumber.value).getOrElse(fail()) shouldBe "0" + genAccountNumber
      }
      "Accept longest possible (8 digits) accountNumber" in {
        val genAccountNumber = numStringGen(8)
        val bandkAccountForm = form.bind(goodData.updated(accountNumber, genAccountNumber))
        bandkAccountForm.errors                                             shouldBe Nil
        bandkAccountForm.value.map(_.accountNumber.value).getOrElse(fail()) shouldBe genAccountNumber
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
