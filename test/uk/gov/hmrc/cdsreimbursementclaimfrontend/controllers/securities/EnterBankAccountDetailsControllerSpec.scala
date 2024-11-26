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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.jsoup.nodes.Document
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MockBankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class EnterBankAccountDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with MockBankAccountReputationService
    with OptionValues {

  val messagesKey: String = "enter-bank-account-details"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[BankAccountReputationService].toInstance(mockBankAccountReputationService)
    )

  val controller: EnterBankAccountDetailsController = instanceOf[EnterBankAccountDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  def validateEnterBankAccountDetailsPage(
    doc: Document,
    expectedBankAccountDetails: Option[BankAccountDetails],
    error: Boolean = false
  ): Assertion = {
    val title         = doc.select("title").first().text()
    val heading       = doc.select(".govuk-heading-xl").eachText().asScala.toList
    val accountName   =
      doc.select("input[name='enter-bank-account-details.account-name']").first().attr("value")
    val sortCode      =
      doc.select("input[name='enter-bank-account-details.sort-code']").first().attr("value")
    val accountNumber =
      doc.select("input[name='enter-bank-account-details.account-number']").first().attr("value")
    title         should ===(
      (if (error) "Error: "
       else "") + "Enter the UK-based bank account details - Claim back import duty and VAT - GOV.UK"
    )
    heading       should ===(List("Enter the UK-based bank account details"))
    accountName   should ===(expectedBankAccountDetails.map(_.accountName.value).getOrElse(""))
    sortCode      should ===(expectedBankAccountDetails.map(_.sortCode.value).getOrElse(""))
    accountNumber should ===(expectedBankAccountDetails.map(_.accountNumber.value).getOrElse(""))
  }

  "Enter Bank Account Details Controller" when {
    "Show Enter Bank Account Details page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "Display the page" in {
        forAll(completeJourneyGen) { journey =>
          val session = SessionData(journey)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateEnterBankAccountDetailsPage(doc, journey.answers.bankAccountDetails)
          )
        }
      }
    }

    "Submit Enter Bank Account Details page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction(Seq.empty)) shouldBe NOT_FOUND
      }

// FIXME
//      "Continue when Mandatory fields are filled and bank account is unavailable" in {
//        forAll(completeJourneyGen, genBankAccountDetails) { case (journey, bankAccountDetails) =>
//          val session                = SessionData(journey)
//          val bankAccountDataToEnter = Seq(
//            "enter-bank-account-details.account-name"   -> bankAccountDetails.accountName.value,
//            "enter-bank-account-details.sort-code"      -> bankAccountDetails.sortCode.value,
//            "enter-bank-account-details.account-number" -> bankAccountDetails.accountNumber.value
//          )
//          journey.answers.bankAccountType
//            .fold {
//              inSequence {
//                mockAuthWithNoRetrievals()
//                mockGetSession(session)
//              }
//              checkIsRedirect(
//                performAction(bankAccountDataToEnter),
//                routes.ChooseBankAccountTypeController.show
//              )
//            } { bankAccountType =>
//              inSequence {
//                mockAuthWithNoRetrievals()
//                mockGetSession(session)
//                mockBankAccountReputation(
//                  bankAccountType,
//                  bankAccountDetails,
//                  None,
//                  Left[ConnectorError, BankAccountReputation](ServiceUnavailableError("this is a mock service"))
//                )
//              }
//              checkIsRedirect(
//                performAction(bankAccountDataToEnter),
//                routes.CheckBankDetailsController.showWarning
//              )
//            }
//        }
//      }
//
//      "Continue when Mandatory fields are filled and bank account is available" in {
//        forAll(completeJourneyGen, genBankAccountDetails) { case (journey, bankAccountDetails) =>
//          val session                = SessionData(journey)
//          val bankAccountDataToEnter = Seq(
//            "enter-bank-account-details.account-name"   -> bankAccountDetails.accountName.value,
//            "enter-bank-account-details.sort-code"      -> bankAccountDetails.sortCode.value,
//            "enter-bank-account-details.account-number" -> bankAccountDetails.accountNumber.value
//          )
//          journey.answers.bankAccountType
//            .fold {
//              inSequence {
//                mockAuthWithNoRetrievals()
//                mockGetSession(session)
//              }
//              checkIsRedirect(
//                performAction(bankAccountDataToEnter),
//                routes.ChooseBankAccountTypeController.show
//              )
//            } { bankAccountType =>
//              inSequence {
//                mockAuthWithNoRetrievals()
//                mockGetSession(session)
//                mockBankAccountReputation(
//                  bankAccountType,
//                  bankAccountDetails,
//                  None,
//                  Right[ConnectorError, BankAccountReputation](
//                    BankAccountReputation(Yes, Some(Yes))
//                  )
//                )
//                mockStoreSession(Right(()))
//              }
//              checkIsRedirect(
//                performAction(bankAccountDataToEnter),
//                routes.CheckBankDetailsController.show
//              )
//            }
//        }
//      }

      "Display errors when account details are blank" in {
        forAll(completeJourneyGen) { journey =>
          whenever(journey.answers.bankAccountType.isDefined) {
            val session                = SessionData(journey)
            val bankAccountDataToEnter = Seq(
              "enter-bank-account-details.account-name"   -> "",
              "enter-bank-account-details.sort-code"      -> "",
              "enter-bank-account-details.account-number" -> ""
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }
            checkPageIsDisplayed(
              performAction(bankAccountDataToEnter),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => {
                validateEnterBankAccountDetailsPage(
                  doc,
                  Some(BankAccountDetails(AccountName(""), SortCode(""), AccountNumber(""))),
                  error = true
                )
                doc
                  .select(".govuk-error-summary__list > li:nth-child(1) > a")
                  .text() should ===("Enter the name on the account")
                doc
                  .select(".govuk-error-summary__list > li:nth-child(2) > a")
                  .text() should ===("Enter a sort code")
                doc
                  .select(".govuk-error-summary__list > li:nth-child(3) > a")
                  .text() should ===("Enter an account number")
              },
              BAD_REQUEST
            )
          }
        }
      }

    }
  }
}
