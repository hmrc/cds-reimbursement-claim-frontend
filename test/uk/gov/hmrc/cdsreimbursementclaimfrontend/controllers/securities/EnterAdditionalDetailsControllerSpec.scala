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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class EnterAdditionalDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterAdditionalDetailsController = instanceOf[EnterAdditionalDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  val session: SessionData = SessionData(SecuritiesJourney.empty(exampleEori))

  private def mockCompleteJourney(journey: SecuritiesJourney) =
    inSequence {
      mockAuthWithDefaultRetrievals()
      mockGetSession(SessionData(journey))
    }

  "Enter Additional Details Controller" when {
    "Enter Additional Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page when additional details given" in {
        forAll(buildCompleteJourneyGen()) { journey =>
          val modifiedJourney   = journey.submitAdditionalDetails("additional details")
          mockCompleteJourney(modifiedJourney)
          val additionalDetails = modifiedJourney.answers.additionalDetails

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-additional-details.securities.title"),
            doc =>
              doc
                .select("form textarea[name='enter-additional-details']")
                .`val`() shouldBe additionalDetails.get
          )
        }
      }

      "display the page when no additional details" in {
        forAll(buildCompleteJourneyGen()) { journey =>
          val modifiedJourney = journey.submitAdditionalDetails("")
          mockCompleteJourney(modifiedJourney)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-additional-details.securities.title"),
            doc =>
              doc
                .select("form textarea[name='enter-additional-details']")
                .`val`() shouldBe ""
          )
        }
      }
    }

    "Submit Enter additional details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "submit valid additional details" in forAll(
        buildJourneyGen()
          .map(_.fold(e => throw new Exception(s"Cannot build complete SecuritiesJourney because of $e."), identity))
      ) { journey =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(
              journey.submitAdditionalDetails("additional details")
            )
          )(Right(()))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-additional-details.securities.title")
        )

        checkIsRedirect(
          performAction(
            "enter-additional-details" -> "additional details"
          ),
          routes.CheckYourAnswersController.show
        )
      }

      "submit valid additional details when journey is complete" in forAll(buildCompleteJourneyGen()) { journey =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            session.copy(securitiesJourney =
              Some(
                journey.submitAdditionalDetails("additional details")
              )
            )
          )(Right(()))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-additional-details.securities.title")
        )

        checkIsRedirect(
          performAction(
            "enter-additional-details" -> "additional details"
          ),
          routes.CheckYourAnswersController.show
        )
      }
    }
  }

}
