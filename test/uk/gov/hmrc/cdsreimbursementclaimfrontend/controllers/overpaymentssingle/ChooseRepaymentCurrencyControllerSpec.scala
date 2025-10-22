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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PayeeTypeGen.arbitraryPayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CurrencyType
import org.jsoup.nodes.Document

class ChooseRepaymentCurrencyControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseRepaymentCurrencyController = instanceOf[ChooseRepaymentCurrencyController]

  val formKey: String = "choose-currency-type"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  def prepareClaim() = {
    val declaration =
      buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("2.00"), true))
      )

    OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
      .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90)))
      .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
      .flatMap(_.submitCorrectAmount(TaxCode.A90, BigDecimal("1.00")))
      .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
      .flatMap(_.submitPayeeType(PayeeType.Consignee))
      .getOrFail
  }

  def assertPageContent(selection: Option[String])(doc: Document) = {
    radioItems(doc) should containOnlyPairsOf(
      Seq(
        m("choose-currency-type.radio.gbp") -> "GBP",
        m("choose-currency-type.radio.eur") -> "EUR"
      )
    )
    hasContinueButton(doc)
  }

  "Choose Currency Type Controller" should {

    def showPage(): Future[Result] = controller.show(FakeRequest())

    def submitPage(selected: String): Future[Result] = controller.submit(
      FakeRequest()
        .withFormUrlEncodedBody("choose-currency-type" -> selected)
    )

    "display page" in {

      val claim = prepareClaim()

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(SessionData(claim))
      }

      checkPageIsDisplayed(
        showPage(),
        messageFromMessageKey(s"$formKey.title"),
        assertPageContent(None)
      )
    }

    "display page with EUR option pre-selected" in {

      val claim =
        prepareClaim()
          .submitCurrencyType(CurrencyType.EUR)
          .getOrFail

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(SessionData(claim))
      }

      checkPageIsDisplayed(
        showPage(),
        messageFromMessageKey(s"$formKey.title"),
        assertPageContent(Some("EUR"))
      )
    }

    "submit EUR option selected" in {

      val claim = prepareClaim()

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(SessionData(claim))
        mockStoreSession(SessionData(claim.submitCurrencyType(CurrencyType.EUR).getOrFail))(Right(()))
      }

      checkIsRedirect(
        submitPage("EUR"),
        routes.EnterBankAccountDetailsController.show
      )

    }

    "submit GBP option selected" in {

      val claim = prepareClaim()

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(SessionData(claim))
        mockStoreSession(SessionData(claim.submitCurrencyType(CurrencyType.GBP).getOrFail))(Right(()))
      }

      checkIsRedirect(
        submitPage("GBP"),
        routes.EnterBankAccountDetailsController.show
      )

    }

  }
}
