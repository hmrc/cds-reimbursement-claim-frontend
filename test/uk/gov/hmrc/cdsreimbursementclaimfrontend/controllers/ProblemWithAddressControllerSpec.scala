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

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.ProblemWithAddressController

class ProblemWithAddressControllerSpec extends ControllerSpec with TableDrivenPropertyChecks {

  lazy val controller: ProblemWithAddressController = instanceOf[ProblemWithAddressController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  "Problem with address controller" must {

    "handling requests to display the problem with address page" must {

      "display the page" in forAll(
        journeys
      ) { journey =>
        checkPageIsDisplayed(
          controller.problem(journey)(FakeRequest()),
          messageFromMessageKey("problem-with-address.title")
        )
      }
    }
  }
}
