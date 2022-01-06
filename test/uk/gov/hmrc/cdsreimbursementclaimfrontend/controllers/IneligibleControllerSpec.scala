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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.i18n.MessagesApi
import play.api.test.FakeRequest

class IneligibleControllerSpec extends ControllerSpec {

  lazy val controller: IneligibleController = instanceOf[IneligibleController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "Ineligible controller" must {

    "handling requests to display the ineligible page" must {

      "display the page" in {
        checkPageIsDisplayed(
          controller.ineligible()(FakeRequest()),
          messageFromMessageKey("ineligible.title")
        )
      }

    }

  }

}
