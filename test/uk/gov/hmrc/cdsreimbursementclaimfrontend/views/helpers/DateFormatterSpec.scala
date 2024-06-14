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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesImpl
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec

class DateFormatterSpec extends AnyWordSpec with ControllerSpec with Matchers {

  private val englishMessages: Messages = MessagesImpl(Lang("en"), theMessagesApi)
  private val welshMessages: Messages   = MessagesImpl(Lang("cy"), theMessagesApi)

  "DateFormatter" should {
    "format date in english correctly - 13 May 2021" in {
      DateFormatter.formatDate("13 May 2021")(englishMessages) shouldBe "13 May 2021"
    }

    "format date in english correctly - 1 August 2020" in {
      DateFormatter.formatDate("1 August 2020")(englishMessages) shouldBe "1 August 2020"
    }

    "format date in welsh correctly - 11 January 2023" in {
      DateFormatter.formatDate("11 January 2023")(welshMessages) shouldBe "11 Ionawr 2023"
    }

    "format date in welsh correctly - 22 November 2021" in {
      DateFormatter.formatDate("22 November 2021")(welshMessages) shouldBe "22 Tachwedd 2021"
    }

  }
}
