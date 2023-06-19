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

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterRejectedGoodsDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.exampleRejectedGoodsDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.journeyWithMrnAndDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

class EnterRejectedGoodsDetailsControllerSpec
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

  val formKey: String = "enter-rejected-goods-details.rejected-goods"

  val controller: EnterRejectedGoodsDetailsController = instanceOf[EnterRejectedGoodsDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "enter-rejected-goods-details.rejected-goods"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  "Enter Rejected Goods Details Controller" must {

    "display the page" when {
      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey(s"$messagesKey.title")
        )
      }
    }

    "handle submit requests" must {
      "the user has entered some details" in {
        val journey        = session.rejectedGoodsMultipleJourney.getOrElse(fail("No rejected goods journey"))
        val updatedJourney = journey.submitDetailsOfRejectedGoods(exampleRejectedGoodsDetails)
        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          controller.submit()(FakeRequest().withFormUrlEncodedBody(formKey -> exampleRejectedGoodsDetails)),
          routes.SelectDutiesController.showFirst
        )
      }
    }

    "show an error summary" when {
      "the user submits empty details" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.submit()(FakeRequest().withFormUrlEncodedBody(formKey -> "")),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "the user submits more than 500 characters" in {
        val answer = List.fill(600)('c').mkString(" ")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.submit()(FakeRequest().withFormUrlEncodedBody(formKey -> answer)),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.maxLength"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

    }
  }

  "Form Validation" must {
    val form     = enterRejectedGoodsDetailsForm
    val goodData = Map(
      messagesKey -> "A box of biscuits"
    )

    "accept rejected goods details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "rejected goods details" should {

      "Accept longest possible details" in {
        val errors = form.bind(goodData.updated(messagesKey, List.fill(500)("a").mkString(""))).errors
        errors shouldBe Nil
      }

      "Reject details when it's too long" in {
        val errors = form.bind(goodData.updated(messagesKey, List.fill(501)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }
  }
}
