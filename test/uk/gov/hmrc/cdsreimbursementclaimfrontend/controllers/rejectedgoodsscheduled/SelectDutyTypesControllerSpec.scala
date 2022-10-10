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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.journeyWithMrnAndDD
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class SelectDutyTypesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutyTypesController = instanceOf[SelectDutyTypesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "select-duty-types"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  "Select Duty Types Controller" when {

    "Show select duty types page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page for the first time" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => selectedCheckBox(doc) shouldBe empty
        )
      }

      "display the page when a duty has already been selected before" in {
        forAll(completeJourneyGen, genDuty) { (journey, dutyType: DutyType) =>
          val updatedJourney = journey.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType)).getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(updatedJourney))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => isCheckboxChecked(doc, dutyType.repr)
          )
        }
      }
    }

    "Submit Select Duty Types page" must {
      def performAction(data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty duty type selection" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
        }

        checkPageIsDisplayed(
          performAction(Seq(messagesKey -> "")),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "select valid duty types when none have been selected before" in forAll { dutyType: DutyType =>
        val updatedJourney = journeyWithMrnAndDD.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType)).getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDD))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(s"$messagesKey[]" -> dutyType.repr)),
          routes.SelectTaxCodesController.show(dutyType)
        )
      }

    }

  }
}
