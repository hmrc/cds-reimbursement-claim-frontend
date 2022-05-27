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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}

class ProblemWithAddressControllerSpec extends ControllerSpec {

  lazy val controller: ProblemWithAddressController = instanceOf[ProblemWithAddressController]

  "Problem with Address Controller" should {
    "Display the page" in {
      checkPageIsDisplayed(
        controller.show()(FakeRequest()),
        messageFromMessageKey("problem-with-address.title"),
        doc =>
          formAction(doc) shouldBe claimsRoutes.CheckContactDetailsMrnController
            .redirectToALF(JourneyBindable.Single)
            .url
      )
    }
  }
}