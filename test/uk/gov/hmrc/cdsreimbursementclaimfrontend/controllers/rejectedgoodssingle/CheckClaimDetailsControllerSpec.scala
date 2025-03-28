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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with ClaimsTableValidator {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  def assertPageContent(
    doc: Document,
    journey: RejectedGoodsSingleJourney
  ): Unit = {

    validateClaimsTableForSingle(
      doc,
      toReimbursementWithCorrectAmount(journey.getReimbursements),
      routes.EnterClaimController.show
    )
    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(
        m("check-claim.selected-duties.question") -> journey.getSelectedDuties
          .getOrElse(Seq.empty)
          .map(taxCode => s"${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}")
          .mkString(" "),
        m("check-claim.table.total")              -> journey.getTotalReimbursementAmount.toPoundSterlingString
      )
    )

    doc
      .getElementsByClass("govuk-summary-list__actions")
      .get(0)
      .getElementsByClass("govuk-link")
      .attr("href")                                     shouldBe routes.CheckClaimDetailsController.redirectToSelectDuties.url
    doc.getElementsByClass("govuk-button").attr("href") shouldBe routes.CheckClaimDetailsController.continue.url
  }

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  private val sessionWithMRN = SessionData(journeyWithMrnAndDeclaration)

  "Check Claim Details Controller" when {
    "Show Check Claim Details page" must {
      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(buildCompleteJourneyGen()) { journey =>
          val session = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claim.title"),
            assertPageContent(_, journey)
          )
        }
      }
    }

    "redirectToSelectDuties" must {

      def performAction(): Future[Result] =
        controller.redirectToSelectDuties(FakeRequest())

      "redirect to select duties page" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionWithMRN)
          mockStoreSession(SessionData(journeyWithMrnAndDeclaration.withDutiesChangeMode(true)))(Right(()))
        }

        checkIsRedirect(performAction(), routes.SelectDutiesController.show)
      }
    }

    "continue to next page" must {

      def performAction(): Future[Result] =
        controller.continue(FakeRequest())

      "redirect to enter inspection date page for incomplete journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionWithMRN)
        }

        checkIsRedirect(performAction(), routes.EnterInspectionDateController.show)
      }

      "continue to cya page for complete journey" in
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }
    }
  }
}
