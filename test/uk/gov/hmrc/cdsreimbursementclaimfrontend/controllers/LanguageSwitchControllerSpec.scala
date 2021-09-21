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

import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

class LanguageSwitchControllerSpec extends AnyWordSpec with Matchers with OptionValues with ScalaFutures {

  val English = "en"
  val Welsh   = "cy"

  "The Language Switch Controller" when {
    "The language switching is disabled" must {

      "Only allow English" in {
        val application = new GuiceApplicationBuilder()
          .configure(
            "enable-language-switching" -> false
          )
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.LanguageSwitchController.switchToLanguage(English).url)

          val result = route(application, request).getOrElse(fail)
          status(result)                               shouldBe SEE_OTHER
          cookies(result).get("PLAY_LANG").value.value shouldBe English
        }
      }

      "Reject Welsh" in {
        val application = new GuiceApplicationBuilder()
          .configure(
            "enable-language-switching" -> false
          )
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.LanguageSwitchController.switchToLanguage(Welsh).url)

          val result = route(application, request).getOrElse(fail)
          status(result)                               shouldBe SEE_OTHER
          cookies(result).get("PLAY_LANG").value.value shouldBe English
        }
      }
    }

    "The language switching is enabled" must {

      "Switch to English when requested" in {
        val application = new GuiceApplicationBuilder()
          .configure(
            "enable-language-switching" -> true
          )
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.LanguageSwitchController.switchToLanguage(English).url)

          val result = route(application, request).getOrElse(fail)
          status(result)                               shouldBe SEE_OTHER
          cookies(result).get("PLAY_LANG").value.value shouldBe English
        }
      }

      "Switch to Welsh when requested" in {
        val application = new GuiceApplicationBuilder()
          .configure(
            "enable-language-switching" -> true
          )
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.LanguageSwitchController.switchToLanguage(Welsh).url)

          val result = route(application, request).getOrElse(fail)
          status(result)                               shouldBe SEE_OTHER
          cookies(result).get("PLAY_LANG").value.value shouldBe Welsh
        }
      }
    }
  }

}
