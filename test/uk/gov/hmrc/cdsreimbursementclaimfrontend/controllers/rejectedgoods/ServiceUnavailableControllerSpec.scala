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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods

import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.BankAccountVerificationUnavailable

class ServiceUnavailableControllerSpec extends ControllerSpec {

  val controller: BankAccountVerificationUnavailable = instanceOf[BankAccountVerificationUnavailable]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "bank-service-unavailable"

  "Service Unavailable Controller" must {

    def performAction(): Future[Result] = controller.show()(FakeRequest())

    "display the page" in {

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$messagesKey.title")
      )
    }

  }
}
