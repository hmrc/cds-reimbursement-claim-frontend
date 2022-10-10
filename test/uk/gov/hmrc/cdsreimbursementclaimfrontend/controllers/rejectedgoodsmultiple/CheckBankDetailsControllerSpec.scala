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
import play.api.test.Helpers.status
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class CheckBankDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with OptionValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckBankDetailsController = instanceOf[CheckBankDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch: FeatureSwitchService = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "bank-details"

  override def beforeEach(): Unit = featureSwitch.enable(Feature.RejectedGoods)

  private def sessionWithBankDetailsInACC14(maybeBankDetails: Option[BankDetails]): SessionData = {
    val displayDeclaration: DisplayDeclaration = displayDeclarationGen.sample.get.withBankDetails(maybeBankDetails)

    val rejectedGoodsMultipleJourney: RejectedGoodsMultipleJourney =
      RejectedGoodsMultipleJourney
        .empty(displayDeclaration.getDeclarantEori)
        .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
        .getOrFail

    session.copy(rejectedGoodsMultipleJourney = Some(rejectedGoodsMultipleJourney))

  }

  private def sessionWithBankDetailsStored(
    session: SessionData,
    bankAccountDetails: BankAccountDetails
  ): SessionData = session.copy(rejectedGoodsMultipleJourney =
    session.rejectedGoodsMultipleJourney.flatMap(_.submitBankAccountDetails(bankAccountDetails).toOption)
  )

  def performAction(): Future[Result] = controller.show()(FakeRequest())

  val session: SessionData = SessionData(journeyWithMrnAndDD)

  "Check Bank Details Controller" must {

    "not find the page if rejected goods feature is disabled" in {
      featureSwitch.disable(Feature.RejectedGoods)
      status(performAction()) shouldBe NOT_FOUND
    }

    "display the page using consignee bank details from Acc14" in forAll(genBankAccountDetails) {
      bankAccountDetails: BankAccountDetails =>
        val consigneeBankDetails: BankDetails = BankDetails(Some(bankAccountDetails), None)
        val sessionWithBankDetails            = sessionWithBankDetailsInACC14(Some(consigneeBankDetails))
        val modifiedSession                   = sessionWithBankDetailsStored(sessionWithBankDetails, bankAccountDetails)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithBankDetails)
          mockStoreSession(modifiedSession)(Right(()))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"bank-details.title"),
          doc =>
            summaryKeyValueMap(doc) shouldBe Map(
              "Name on the account" -> bankAccountDetails.accountName.value,
              "Sort code"           -> bankAccountDetails.sortCode.masked(MessagesImpl(Lang("en"), theMessagesApi)),
              "Account number"      -> bankAccountDetails.accountNumber.masked(MessagesImpl(Lang("en"), theMessagesApi))
            )
        )

    }

    "display the page using declarant bank details from Acc14" in forAll(genBankAccountDetails) {
      bankAccountDetails: BankAccountDetails =>
        val declarantBankDetails: BankDetails = BankDetails(None, Some(bankAccountDetails))
        val sessionWithBankDetails            = sessionWithBankDetailsInACC14(Some(declarantBankDetails))
        val modifiedSession                   = sessionWithBankDetailsStored(sessionWithBankDetails, bankAccountDetails)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithBankDetails)
          mockStoreSession(modifiedSession)(Right(()))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"bank-details.title"),
          doc =>
            summaryKeyValueMap(doc) shouldBe Map(
              "Name on the account" -> bankAccountDetails.accountName.value,
              "Sort code"           -> bankAccountDetails.sortCode.masked(MessagesImpl(Lang("en"), theMessagesApi)),
              "Account number"      -> bankAccountDetails.accountNumber.masked(MessagesImpl(Lang("en"), theMessagesApi))
            )
        )

    }

    "display the page with submitted bank details" in forAll(genBankAccountDetails) { bankDetails: BankAccountDetails =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithBankDetailsStored(session, bankDetails))
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$messagesKey.title"),
        doc =>
          summaryKeyValueMap(doc) shouldBe Map(
            "Name on the account" -> bankDetails.accountName.value,
            "Sort code"           -> bankDetails.sortCode.masked(MessagesImpl(Lang("en"), theMessagesApi)),
            "Account number"      -> bankDetails.accountNumber.masked(MessagesImpl(Lang("en"), theMessagesApi))
          )
      )
    }

    "display the page on a pre-existing journey" in forAll(
      buildCompleteJourneyGen()
    ) { journey =>
      val sessionToAmend = session.copy(rejectedGoodsMultipleJourney = Some(journey))
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionToAmend)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$messagesKey.title")
      )
    }

    "Redirect when BankDetails are empty" in {
      val bankDetails = BankDetails(None, None)
      val session     = sessionWithBankDetailsInACC14(Some(bankDetails))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(),
        routes.ChooseBankAccountTypeController.show()
      )
    }

  }

}
