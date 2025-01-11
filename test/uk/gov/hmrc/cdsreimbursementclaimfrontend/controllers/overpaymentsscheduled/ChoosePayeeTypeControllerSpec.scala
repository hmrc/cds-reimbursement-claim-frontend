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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import org.scalatest.BeforeAndAfterEach
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, PropertyBasedControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PayeeTypeGen.arbitraryPayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Feature, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class ChoosePayeeTypeControllerSpec
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

  val controller: ChoosePayeeTypeController = instanceOf[ChoosePayeeTypeController]

  val formKey: String = "choose-payee-type"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.Overpayments_v2

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  "Choose Payee Type Controller" should {

    def showPage(): Future[Result] =
      controller.show(FakeRequest())

    def submitPayeeType(data: (String, String)*): Future[Result] =
      controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

    "display page" in forAll { (maybePayeeType: Option[PayeeType]) =>
      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(
          maybePayeeType.toList.foldLeft(session)((session, payeeType) =>
            session.copy(overpaymentsScheduledJourney =
              journeyWithMrnAndDeclaration.submitPayeeType(payeeType).toOption
            )
          )
        )
      }

      checkPageIsDisplayed(
        showPage(),
        messageFromMessageKey(s"$formKey.title")
      )
    }

    "fail to submit payee type" when {
      "nothing is selected" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitPayeeType(),
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
      "one of the options selected" in forAll { (payeeType: PayeeType) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(
              overpaymentsScheduledJourney = journeyWithMrnAndDeclaration.submitPayeeType(payeeType).toOption
            )
          )(Right(()))
        }

        checkIsRedirect(
          submitPayeeType(formKey -> payeeType.toString),
          routes.EnterBankAccountDetailsController.show
        )
      }
    }
  }
}
