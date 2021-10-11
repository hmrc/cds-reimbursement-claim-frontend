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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import scala.concurrent.Future
import play.api.test.Helpers._

class CheckReimbursementClaimControllerSpec extends ControllerSpec {

  lazy val controller: CheckReimbursementClaimController = instanceOf[CheckReimbursementClaimController]

  "Check Reimbursement Claim Controller" should {

    def performAction: Future[Result] = controller.showReimbursementClaim()(FakeRequest())

    "display the page" in {
      val result = performAction
      status(result)          shouldBe OK
      contentAsString(result) shouldBe "implementation todo ...."
    }
  }
}
