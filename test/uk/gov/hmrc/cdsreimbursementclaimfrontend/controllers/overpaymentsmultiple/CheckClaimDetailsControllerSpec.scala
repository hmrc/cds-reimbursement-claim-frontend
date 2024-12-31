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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.CheckClaimDetailsController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with SummaryMatchers
    with BeforeAndAfterEach
    with ClaimsTableValidator {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "check-claim.multiple"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  def assertPageContent(
    doc: Document,
    journey: OverpaymentsMultipleJourney
  ): Unit = {

    validateClaimsTablesForMultiple(
      doc,
      journey.getReimbursementsWithCorrectAmounts,
      routes.EnterClaimController.show
    )

    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(m("check-claim.table.total") -> journey.getTotalReimbursementAmount.toPoundSterlingString)
    )

    assertPageElementsByIdAndExpectedText(doc)(
      s"check-claim-yes-no" -> s"${m(s"check-claim.is-this-correct")} ${m(s"check-claim.yes")} ${m(s"check-claim.no")}"
    )
  }

  val journeyGen: Gen[(OverpaymentsMultipleJourney, Seq[MRN])] =
    incompleteJourneyWithCompleteClaimsGen(5)

  "CheckClaimDetailsController" when {

    "Show" must {
      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "not find the page if overpayments_v2 feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(journeyGen) { case (journey, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              s"$messagesKey.title"
            ),
            doc => assertPageContent(doc, journey)
          )
        }
      }
    }

    "Submit" must {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "accept YES response and redirect to the next page" in
        forAll(journeyGen) { case (journey, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("check-claim" -> "true"),
            routes.ChoosePayeeTypeController.show
          )
        }

      "accept YES response and redirect to the CYA page when in change mode" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("check-claim" -> "true"),
            routes.CheckYourAnswersController.show
          )
        }

      "accept NO response and redirect to select duties page" in {
        val (journey, _) = journeyGen.sample.get
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
        }

        checkIsRedirect(
          performAction("check-claim" -> "false"),
          routes.SelectDutiesController.showFirst
        )
      }
    }
  }
}
