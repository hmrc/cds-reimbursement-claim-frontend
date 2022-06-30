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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.Helpers.status
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectSecuritiesController.selectSecuritiesKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.EitherOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.exampleMrn
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.exampleSecuritiesDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class SelectSecuritiesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: SelectSecuritiesController = instanceOf[SelectSecuritiesController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = selectSecuritiesKey
  private val id: String          = "something"
  val journey: SecuritiesJourney  = SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn)

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  "SelectSecuritiesController" when {

    "show page" must {

      def performAction(): Future[Result] = controller.show(id)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page for the first time" in {
        val initialJourney =
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(
              exampleSecuritiesDisplayDeclaration.getReasonForSecurity.get,
              exampleSecuritiesDisplayDeclaration
            )
            .getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title")
        )
      }

    }

  }

}
