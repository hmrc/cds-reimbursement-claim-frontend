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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import org.jsoup.nodes.Document
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
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

  private val messagesKey: String = "check-claim.rejected-goods"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def assertPageContent(
    doc: Document,
    journey: RejectedGoodsMultipleJourney
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

  "CheckClaimDetailsController" when {

    "Show claim summary" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if all selected claims provided" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(9)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title"),
            doc => assertPageContent(doc, journey)
          )
        }
      }
    }

    "Submit" must {

      def performAction(value: String): Future[Result] =
        controller.submit(
          FakeRequest()
            .withFormUrlEncodedBody("check-claim" -> value)
        )

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction("true"))  shouldBe NOT_FOUND
        status(performAction("false")) shouldBe NOT_FOUND
      }

      "redirect to the next page if answer is yes" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(2)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("true"),
            "/claim-back-import-duty-vat/rejected-goods/multiple/enter-inspection-date"
          )
        }
      }

      "redirect back to the duties selection if answer is no" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(2)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
          }

          checkIsRedirect(
            performAction("false"),
            "/claim-back-import-duty-vat/rejected-goods/multiple/select-duties"
          )
        }
      }

      "when in change mode redirect to the CYA page if answer is yes" in {
        forAll(completeJourneyGen) { journey =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("true"),
            "/claim-back-import-duty-vat/rejected-goods/multiple/check-your-answers"
          )
        }
      }

      "when in change mode redirect back to the duties selection if answer is no" in {
        forAll(completeJourneyGen) { journey =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
          }

          checkIsRedirect(
            performAction("false"),
            "/claim-back-import-duty-vat/rejected-goods/multiple/select-duties"
          )
        }
      }
    }
  }

}
