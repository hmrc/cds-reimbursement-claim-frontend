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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

class EnterMrnMultiControllerSpec extends ControllerSpec {

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  lazy val controller: EnterMrnMultiController = instanceOf[EnterMrnMultiController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  " EnterMrnMultiPlaceholderController" must {

    "handling requests to display the enter mrn schedule placeholder page" must {

      "redirect to the error page" when {
        "the feature switch bulk claim is disabled" in {
          featureSwitch.BulkClaim.disable()
          val result = controller.show()(FakeRequest())
          status(result) shouldBe NOT_FOUND
        }
      }

      "display the page" in {
        featureSwitch.BulkClaim.enable()
        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-lead-mrn-or-entry-number.title")
        )
      }

    }

  }

}
