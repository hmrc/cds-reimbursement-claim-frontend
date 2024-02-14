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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.journeyWithMrnAndDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
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
    featureSwitch.enable(Feature.Overpayments_v2)

  "Select Duty Types Controller" when {

    "Show select duty types page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "not find the page if overpayments v2 feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page for the first time" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedCheckBox(doc)                                    shouldBe empty
            doc.getElementById("select-duty-types").`val`()          shouldBe "uk-duty"
            doc.getElementById("select-duty-types-2").`val`()        shouldBe "eu-duty"
            doc.getElementById("select-duty-types-excise").`val`()   shouldBe "beer"
            doc.getElementById("select-duty-types-excise-2").`val`() shouldBe "wine"
          }
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
        controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if Overpayments v2 feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty duty type selection" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
        }

        checkPageIsDisplayed(
          performAction(Seq(messagesKey -> "")),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "select valid duty types when none have been selected before" in forAll { dutyType: DutyType =>
        val updatedJourney =
          journeyWithMrnAndDeclaration.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType)).getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journeyWithMrnAndDeclaration))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(s"$messagesKey[]" -> dutyType.repr)),
          routes.SelectDutiesController.show(dutyType)
        )
      }

    }

  }
}
