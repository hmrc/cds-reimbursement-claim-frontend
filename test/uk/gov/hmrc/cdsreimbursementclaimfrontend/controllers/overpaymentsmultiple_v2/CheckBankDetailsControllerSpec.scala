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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2

import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
import play.api.i18n.Lang
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

class CheckBankDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with MockFactory
    with OptionValues
    with BeforeAndAfterEach {

  val claimService: ClaimService = mock[ClaimService]

  lazy val controller: CheckBankDetailsController = instanceOf[CheckBankDetailsController]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(claimService)
    )

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Overpayments_v2)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  private def sessionWithBankDetailsInACC14(
    maybeBankDetails: Option[BankDetails],
    payeeType: Option[PayeeType] = None
  ): SessionData = {
    val displayDeclaration: DisplayDeclaration =
      displayDeclarationGen.sample.get.withBankDetails(maybeBankDetails)

    val overpaymentsMultipleJourney: OverpaymentsMultipleJourney =
      OverpaymentsMultipleJourney
        .empty(displayDeclaration.getDeclarantEori)
        .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
        .flatMapWhenDefined(payeeType)(journey => payeeType => journey.submitPayeeType(payeeType))
        .getOrFail

    SessionData.empty.copy(
      overpaymentsMultipleJourney = Some(overpaymentsMultipleJourney)
    )
  }

  private def sessionWithBankDetailsStored(
    session: SessionData,
    bankAccountDetails: BankAccountDetails
  ): SessionData =
    session.copy(overpaymentsMultipleJourney =
      session.overpaymentsMultipleJourney
        .flatMap(_.submitBankAccountDetails(bankAccountDetails).toOption)
    )

  "Check Bank Details Controller" when {

    "Check Bank Account Details" should {

      "Redirect when BankDetails is empty and required" in {
        val bankDetails = BankDetails(None, None)
        val session     = sessionWithBankDetailsInACC14(Some(bankDetails))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.show()(request)

        checkIsRedirect(result, routes.ChooseBankAccountTypeController.show)

      }

      "Redirect when BankDetails is None and required" in {
        val session = sessionWithBankDetailsInACC14(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.show()(request)
        checkIsRedirect(result, routes.ChooseBankAccountTypeController.show)
      }

      "Ok when BankDetails has consigneeBankDetails and payeeType is Consignee" in forAll(genBankAccountDetails) {
        consigneeBankDetails: BankAccountDetails =>
          val bankDetails     = BankDetails(Some(consigneeBankDetails), None)
          val session         = sessionWithBankDetailsInACC14(Some(bankDetails), Some(PayeeType.Consignee))
          val modifiedSession = sessionWithBankDetailsStored(session, consigneeBankDetails)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(modifiedSession)(Right(()))
          }
          val request         = FakeRequest()
          val result          = controller.show()(request)

          checkPageIsDisplayed(
            result,
            messageFromMessageKey("bank-details.title"),
            doc =>
              summaryKeyValueMap(doc) shouldBe Map(
                "Name on the account" -> consigneeBankDetails.accountName.value,
                "Sort code"           -> consigneeBankDetails.sortCode.masked(MessagesImpl(Lang("en"), theMessagesApi)),
                "Account number"      -> consigneeBankDetails.accountNumber.masked(MessagesImpl(Lang("en"), theMessagesApi))
              )
          )
      }

      "Ok when BankDetails has declarantBankDetails and payeeType is Declarant" in forAll(genBankAccountDetails) {
        declarantBankDetails: BankAccountDetails =>
          val bankDetails     = BankDetails(None, Some(declarantBankDetails))
          val session         = sessionWithBankDetailsInACC14(Some(bankDetails), Some(PayeeType.Declarant))
          val modifiedSession = sessionWithBankDetailsStored(session, declarantBankDetails)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(modifiedSession)(Right(()))
          }
          val request         = FakeRequest()
          val result          = controller.show()(request)

          checkPageIsDisplayed(
            result,
            messageFromMessageKey("bank-details.title"),
            doc =>
              summaryKeyValueMap(doc) shouldBe Map(
                "Name on the account" -> declarantBankDetails.accountName.value,
                "Sort code"           -> declarantBankDetails.sortCode.masked(MessagesImpl(Lang("en"), theMessagesApi)),
                "Account number"      -> declarantBankDetails.accountNumber.masked(MessagesImpl(Lang("en"), theMessagesApi))
              )
          )
      }

      "Ok when in change mode" in forAll(completeJourneyGen) { journey =>
        val session            = SessionData(journey)
        val bankAccountDetails = journey.answers.bankAccountDetails.get
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request            = FakeRequest()
        val result             = controller.show()(request)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("bank-details.title"),
          doc =>
            summaryKeyValueMap(doc) shouldBe Map(
              "Name on the account" -> bankAccountDetails.accountName.value,
              "Sort code"           -> bankAccountDetails.sortCode.masked(MessagesImpl(Lang("en"), theMessagesApi)),
              "Account number"      -> bankAccountDetails.accountNumber.masked(MessagesImpl(Lang("en"), theMessagesApi))
            )
        )
      }
    }
  }
}
