package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MockBankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.arbitraryBankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.genReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen.genBankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaNumGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.numStringGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
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

  implicit val request: Request[_] = FakeRequest()

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "enter-bank-account-details"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  "Enter Bank Account Details Controller" must {
    "not find the page if overpayments feature is disabled" in {
      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      featureSwitch.disable(Feature.Overpayments_v2)

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

      "the user has answered this question before" in forAll(completeJourneyGen) { journey =>
        val updatedSession = session.copy(overpaymentsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
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
    }
    "validate bank account details" when {
      val journey: OverpaymentsMultipleJourney = completeJourneyGen.sample.get

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

      "redirect to choose bank account type page if no bank account type present in session" in
        forAll(
          genBankAccountDetails
        ) { bankDetails =>
          checkIsRedirect(
            validatedResult(bankDetails),
            routes.ChooseBankAccountTypeController.show
          )
        }

      "personal account" must {
        "redirect to the check bank accounts page if a personal account that exists with a valid sort code is specified" in
          forAll(
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
              mockBankAccountReputation(BankAccountType.Personal, bankDetails, postCode, Right(personalResponse))
            )

            checkIsRedirect(
              validatedResult(bankDetails, Some(BankAccountType.Personal), postCode),
              routes.CheckBankDetailsController.show
            )
          }

        "show error on the the bank account details page if there is an error response" in
          forAll(
            genBankAccountDetails,
            Gen.option(genPostcode),
            arbitraryBankAccountReputation.arbitrary
          ) { (bankAccountDetails, postCode, genericResponse) =>
            val expectedResponse = genericResponse.copy(
              otherError = Some(ReputationErrorResponse("account-does-not-exist", "error"))
            )

            inSequence(
              mockBankAccountReputation(BankAccountType.Personal, bankAccountDetails, postCode, Right(expectedResponse))
            )

            checkPageIsDisplayed(
              validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
              messageFromMessageKey(s"$messagesKey.title"),
              doc =>
                getErrorSummary(doc) shouldBe messageFromMessageKey(
                  "enter-bank-account-details.error.account-does-not-exist"
                ),
              BAD_REQUEST
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
              mockBankAccountReputation(BankAccountType.Personal, bankAccountDetails, postCode, Right(expectedResponse))
            )

            checkPageIsDisplayed(
              validatedResult(bankAccountDetails, Some(BankAccountType.Personal), postCode),
              messageFromMessageKey(s"$messagesKey.title"),
              doc =>
                getErrorSummary(doc) shouldBe messageFromMessageKey(
                  "enter-bank-account-details.error.account-exists-error"
                ),
              BAD_REQUEST
            )
          }
      }
    }
  }
}
