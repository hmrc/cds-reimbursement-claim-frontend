/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class DisposalMethodControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with OptionValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: DisposalMethodController = instanceOf[DisposalMethodController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData(journeyWithMrnAndDD)

  "Disposal Method Controller" when {
    "Disposal Method page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-method-of-disposal.rejected-goods.title"),
          doc => {
            doc
              .select("body > div > main > div > div > h1")
              .text()               shouldBe messageFromMessageKey("select-method-of-disposal.rejected-goods.title")
            selectedRadioValue(doc) shouldBe None
          }
        )
      }
    }

    "Disposal Method page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an empty disposal method" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction("select-method-of-disposal.rejected-goods.error.unknown" -> ""),
          messageFromMessageKey("select-method-of-disposal.rejected-goods.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "select-method-of-disposal.rejected-goods.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid disposal method" in forAll(Gen.oneOf(MethodOfDisposal.values)) { methodOfDisposal =>
        val journey        = session.rejectedGoodsScheduledJourney.getOrElse(fail("No rejected goods journey"))
        val updatedJourney = journey.submitMethodOfDisposal(methodOfDisposal)
        val updatedSession = session.copy(rejectedGoodsScheduledJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("select-method-of-disposal.rejected-goods" -> methodOfDisposal.toString),
          routes.EnterRejectedGoodsDetailsController.show()
        )
      }

      "redirect to CYA page" when {
        "journey is complete" in forAll(buildCompleteJourneyGen()) { journey =>
          val sessionWitJourney = session.copy(rejectedGoodsScheduledJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWitJourney)
          }

          checkIsRedirect(
            performAction(
              "select-method-of-disposal.rejected-goods" ->
                journey.answers.methodOfDisposal.map(_.toString).value
            ),
            routes.CheckYourAnswersController.show()
          )
        }
      }
    }
  }

}
