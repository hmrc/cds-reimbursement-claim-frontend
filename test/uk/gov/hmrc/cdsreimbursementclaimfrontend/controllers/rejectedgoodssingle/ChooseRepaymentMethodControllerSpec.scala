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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import org.jsoup.nodes.Document
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class ChooseRepaymentMethodControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseRepaymentMethodController = instanceOf[ChooseRepaymentMethodController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  def assertPageContent(
    doc: Document
  ): Unit = {
    radioItems(doc) should containOnlyPairsOf(
      Seq(
        m("reimbursement-method.cma")           -> "0",
        m("reimbursement-method.bank-transfer") -> "1"
      )
    )
    hasContinueButton(doc)
  }

  val journeyCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildJourneyFromAnswersGen(
      buildAnswersGen(
        allDutiesCmaEligible = true,
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        reimbursementMethod = None
      )
    )

  val journeyNotCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildJourneyFromAnswersGen(
      buildAnswersGen(
        allDutiesCmaEligible = false,
        submitBankAccountDetails = false,
        submitBankAccountType = false,
        reimbursementMethod = None
      )
    )

  "Repayment Method Controller" when {

    "Show repayment method page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page if all duties are are CMA eligible" in
        forAll(journeyCMAEligibleGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("reimbursement-method.title"),
            assertPageContent(_)
          )
        }

      "redirect to enter bank details page if not all duties are CMA eligible" in
        forAll(journeyNotCMAEligibleGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterBankAccountDetailsController.show
          )
        }
    }

    "Submit Repayment Method" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data*)
        )

      "accept selection of Current Method Adjustment" in
        forAll(journeyCMAEligibleGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(
              SessionData(
                journey
                  .submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment)
                  .getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("reimbursement-method" -> "0"),
            routes.EnterBankAccountDetailsController.show
          )
        }

      "accept selection of Bank Account Transfer" in
        forAll(journeyCMAEligibleGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(
              SessionData(
                journey
                  .submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer)
                  .getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("reimbursement-method" -> "1"),
            routes.EnterBankAccountDetailsController.show
          )
        }

      "reject invalid selection" in
        forAll(journeyCMAEligibleGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction("reimbursement-method" -> "2"),
            messageFromMessageKey("reimbursement-method.title"),
            doc => {
              assertPageContent(doc)
              assertShowsInputError(doc, Some(m("reimbursement-method.error.invalid")))
            },
            expectedStatus = BAD_REQUEST
          )
        }

      "reject empty selection" in
        forAll(journeyCMAEligibleGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("reimbursement-method.title"),
            doc => {
              assertPageContent(doc)
              assertShowsInputError(doc, Some(m("reimbursement-method.error.required")))
            },
            expectedStatus = BAD_REQUEST
          )
        }
    }

  }
}
