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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.concurrent.Future

class ChooseBankAccountTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with TestWithJourneyGenerator[SecuritiesJourney] {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val session: SessionData = SessionData.empty.copy(
    securitiesJourney = Some(emptyJourney)
  )

  val controller: ChooseBankAccountTypeController = instanceOf[ChooseBankAccountTypeController]

  val formKey: String = "select-bank-account-type"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.Securities

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  "Choose Bank Account Type Controller" should {

    def showPage(): Future[Result] =
      controller.show()(FakeRequest())

    def submitBankAccountType(data: (String, String)*): Future[Result] =
      controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    "display page" in forAllWith(
      JourneyGenerator(
        testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsNotGuaranteeEligibleGen,
        journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
      )
    ) { case (initialJourney: SecuritiesJourney, _) =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(securitiesJourney = Some(initialJourney)))
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
      "one of the options selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsNotGuaranteeEligibleGen,
          journeyBuilder = buildSecuritiesJourneyReadyForEnteringClaimAmounts
        )
      ) { case (initialJourney: SecuritiesJourney, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(securitiesJourney = Some(initialJourney)))
          mockStoreSession(
            session.copy(
              securitiesJourney = Some(
                initialJourney
                  .submitBankAccountType(BankAccountType.Personal)
                  .getOrFail
              )
            )
          )(Right(()))
        }

        checkIsRedirect(
          submitBankAccountType(formKey -> BankAccountType.Personal.toString),
          routes.EnterBankAccountDetailsController.show()
        )
      }
    }
  }
}
