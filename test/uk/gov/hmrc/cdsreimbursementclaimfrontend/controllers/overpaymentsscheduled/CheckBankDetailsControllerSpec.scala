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

import org.jsoup.Jsoup
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class CheckBankDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckBankDetailsController = instanceOf[CheckBankDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  val messagesKey: String = "bank-details"

  "Check Bank Details Controller" when {
    "Check Bank Details page" must {

      def performAction(): Future[Result] = controller.showWarning(FakeRequest())

      "display the page" in {
        val journey        = completeJourneyGen.sample.getOrElse(fail("Journey building has failed."))
        val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.warning.title"),
          doc => {

            doc
              .select("main p")
              .get(0)
              .text() shouldBe Jsoup.parse(messageFromMessageKey(s"$messagesKey.warning.inset")).text()
            doc
              .select(".govuk-summary-list__row dt.govuk-summary-list__key")
              .get(0)
              .text() shouldBe "Name on the account"
            doc
              .select(".govuk-summary-list__row dd.govuk-summary-list__value")
              .get(0)
              .text() shouldBe claim.bankAccountDetails.map(_.accountName).get.value
            doc
              .select(".govuk-summary-list__row dt.govuk-summary-list__key")
              .get(1)
              .text() shouldBe "Sort code"
            doc
              .select(".govuk-summary-list__row dd.govuk-summary-list__value")
              .get(1)
              .text() shouldBe claim.bankAccountDetails.map(_.sortCode).get.value
            doc
              .select(".govuk-summary-list__row dt.govuk-summary-list__key")
              .get(2)
              .text() shouldBe "Account number"
            doc
              .select(".govuk-summary-list__row dd.govuk-summary-list__value")
              .get(2)
              .text() shouldBe claim.bankAccountDetails.map(_.accountNumber).get.value
          }
        )
      }

      "redirect to enter bank account details page when no bank account details provided" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.EnterBankAccountDetailsController.show
        )
      }
    }

    "Submit Check Bank Details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submitWarning(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty Yes/No answer" in {
        val journey = journeyWithMrnAndDeclaration
          .submitBankAccountDetails(exampleBankAccountDetails)
          .getOrElse(fail("Journey building has failed."))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageWithErrorIsDisplayed(
          performAction("bank-details" -> ""),
          messageFromMessageKey(s"$messagesKey.warning.title"),
          messageFromMessageKey(s"$messagesKey.error.required")
        )
      }

      "submit when user selects Yes" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(
              journeyWithMrnAndDeclaration
                .submitBankAccountDetails(exampleBankAccountDetails)
                .getOrElse(fail("Journey building has failed."))
            )
          )
        }

        checkIsRedirect(
          performAction("bank-details" -> "true"),
          routes.ChooseFileTypeController.show
        )
      }

      "submit when user selects No" in {
        val sessionToAmend = SessionData(
          journeyWithMrnAndDeclaration
            .submitBankAccountDetails(exampleBankAccountDetails)
            .getOrElse(fail("Journey building has failed."))
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
          mockStoreSession(
            sessionToAmend.copy(
              overpaymentsScheduledJourney =
                sessionToAmend.overpaymentsScheduledJourney.map(_.removeBankAccountDetails())
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("bank-details" -> "false"),
          routes.EnterBankAccountDetailsController.show
        )
      }

      "redirect to CYA page when user has seen CYA page" in {
        val journey = completeJourneyGen.sample
          .getOrElse(fail("Journey building has failed."))
          .submitCheckYourAnswersChangeMode(true)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction("bank-details" -> "true"),
          routes.CheckYourAnswersController.show
        )
      }

      "return status 500 when submitting empty Yes/No answer without bank details" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        status(performAction("bank-details" -> "")) shouldBe INTERNAL_SERVER_ERROR
      }
    }
  }
}
