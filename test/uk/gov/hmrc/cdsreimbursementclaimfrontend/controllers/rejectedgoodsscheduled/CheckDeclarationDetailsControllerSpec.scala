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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.jsoup.Jsoup
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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class CheckDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

  val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  implicit val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.RejectedGoods)

  val session =
    SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(RejectedGoodsScheduledJourney.empty(exampleEori)))

  val messagesKey: String = "check-declaration-details"

  "Check Declaration Details Controller" when {
    "Check Declaration Details page" must {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "does not find the page if the rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in forAll(
        buildCompleteJourneyGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false
        )
      ) { journey =>
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            val expectedMainParagraph = Jsoup.parse(messageFromMessageKey(s"$messagesKey.help-text")).text()

            doc
              .select("main p")
              .get(0)
              .text()                                    shouldBe expectedMainParagraph
            doc.select(s"#$messagesKey").attr("checked") shouldBe ""
          }
        )
      }
    }

    "Submit Check Declaration Details page" must {

      def performAction(data: (String, String)*)(implicit
        controller: CheckDeclarationDetailsController
      ): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Yes/No answer" in {
        val displayDeclaration = sample[DisplayDeclaration]
        val journey            = session.rejectedGoodsScheduledJourney.get
          .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
          .getOrFail
        val sessionToAmend     = session.copy(rejectedGoodsScheduledJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction("check-declaration-details" -> ""),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey(s"$messagesKey.error.required")
            doc.select(s"#$messagesKey").attr("checked") shouldBe ""
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit when user selects Yes" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction("check-declaration-details" -> "true"),
          routes.UploadMrnListController.show()
        )
      }

      "submit when user selects No" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction("check-declaration-details" -> "false"),
          routes.EnterMovementReferenceNumberController.submit()
        )

      }

    }
  }

}
