/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{JourneyStatus, SessionData}

import scala.concurrent.Future

trait RedirectToStartBehaviour {
  this: ControllerSpec with AuthSupport with SessionSupport =>

  def redirectToStartWhenInvalidJourney(
    performAction: () => Future[Result],
    isValidJourneyStatus: JourneyStatus => Boolean
  ): Unit =
    "redirect to the start endpoint" when {

      "there is no journey status in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkIsRedirect(
          performAction(),
          controllers.routes.StartController.start()
        )
      }

      "the journey status in session is not valid" in new ScalaCheckDrivenPropertyChecks {
        forAll { j: JourneyStatus =>
          whenever(!isValidJourneyStatus(j)) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData.empty.copyWith(journeyStatus = Some(j)))
            }

            checkIsRedirect(
              performAction(),
              controllers.routes.StartController.start()
            )
          }
        }
      }

    }

}
