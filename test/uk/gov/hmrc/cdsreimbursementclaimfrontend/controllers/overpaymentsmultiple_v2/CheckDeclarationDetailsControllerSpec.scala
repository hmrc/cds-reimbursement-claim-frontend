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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class CheckDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with JourneyTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Overpayments_v2)

  val session =
    SessionData.empty.copy(overpaymentsMultipleJourney = Some(OverpaymentsMultipleJourney.empty(exampleEori)))

  val messagesKey: String = "check-declaration-details"

  def performAction(data: (String, String)*)(implicit controller: CheckDeclarationDetailsController): Future[Result] =
    controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

  "Check Declaration Details Controller" when {
    "Check Declaration Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "does not find the page if the rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
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
          doc => doc.select(s"#$messagesKey").attr("checked") shouldBe ""
        )
      }

      "redirect to the enter movement reference if no declaration present" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.IneligibleController.ineligible()
        )
      }
    }

    "Submit Check Declaration Details page" must {

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Yes/No answer" in forAll { displayDeclaration: DisplayDeclaration =>
        val journey        = session.overpaymentsMultipleJourney.get
          .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
          .getOrFail
        val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

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
          routes.EnterMovementReferenceNumberController.show(1)
        )
      }

      "submit when user selects No" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction("check-declaration-details" -> "false"),
          routes.EnterMovementReferenceNumberController.showFirst
        )
      }
    }
  }
}
