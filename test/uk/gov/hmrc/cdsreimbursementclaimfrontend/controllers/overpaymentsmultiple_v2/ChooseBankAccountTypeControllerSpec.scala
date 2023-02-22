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

import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.arbitraryBankAccountType

import scala.concurrent.Future

class ChooseBankAccountTypeControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  val controller: ChooseBankAccountTypeController = instanceOf[ChooseBankAccountTypeController]

  val formKey: String = "select-bank-account-type"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.Overpayments_v2

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  "Choose Bank Account Type Controller" should {

    def showPage(): Future[Result] =
      controller.show()(FakeRequest())

    def submitBankAccountType(data: (String, String)*): Future[Result] =
      controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    "display page" in forAll { (maybeBankAccountType: Option[BankAccountType]) =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          maybeBankAccountType.toList.foldLeft(session)((session, bankAccountType) =>
            session.copy(overpaymentsMultipleJourney =
              journeyWithMrnAndDeclaration.submitBankAccountType(bankAccountType).toOption
            )
          )
        )
      }

      checkPageIsDisplayed(
        showPage(),
        messageFromMessageKey(s"$formKey.title")
      )
    }

    "fail to submit bank account type" when {
      "nothing is selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitBankAccountType(),
          messageFromMessageKey(s"$formKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$formKey.error.required"
            ),
          BAD_REQUEST
        )
      }
    }

    "successfully submit bank account type" when {
      "one of the options selected" in forAll { bankAccountType: BankAccountType =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(
              overpaymentsMultipleJourney = journeyWithMrnAndDeclaration.submitBankAccountType(bankAccountType).toOption
            )
          )(Right(()))
        }

        checkIsRedirect(
          submitBankAccountType(formKey -> bankAccountType.toString),
          routes.EnterBankAccountDetailsController.show
        )
      }
    }
  }
}
