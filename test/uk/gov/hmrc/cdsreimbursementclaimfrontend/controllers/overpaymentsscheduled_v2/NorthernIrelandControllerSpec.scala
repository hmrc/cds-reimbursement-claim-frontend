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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, PropertyBasedControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Feature, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class NorthernIrelandControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: NorthernIrelandController = instanceOf[NorthernIrelandController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Overpayments_v2)

  val journeyGen: Gen[OverpaymentsScheduledJourney] =
    buildJourneyGen(answersUpToBasisForClaimGen())

  "Northern Ireland Controller" when {
    "Northern Ireland page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display page the first time" in {
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("claim-northern-ireland.title")
          )
        }
      }

      "display page back with details populated" in {
        forAll(journeyGen.map(_.submitAdditionalDetails("foo bar 123"))) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("claim-northern-ireland.title")
          )
        }
      }

      "display page back in the change mode" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("claim-northern-ireland.title"),
            x => x.getElementById("claim-northern-ireland-hint").text() should ===(
              messageFromMessageKey("claim-northern-ireland.scheduled.help-text")
            )
          )
        }
      }
    }

    "Norther Ireland choice" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "submit user choice YES" in forAll(journeyGen) { journey =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(journey.submitWhetherNorthernIreland(true))
          )(Right(()))
        }

        checkIsRedirect(
          performAction(
            "claim-northern-ireland" -> "true"
          ),
          routes.BasisForClaimController.show
        )
      }

      "submit user choice NO" in forAll(journeyGen) { journey =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(journey.submitWhetherNorthernIreland(false))
          )(Right(()))
        }

        checkIsRedirect(
          performAction(
            "claim-northern-ireland" -> "false"
          ),
          routes.BasisForClaimController.show
        )
      }

    }
  }

}
