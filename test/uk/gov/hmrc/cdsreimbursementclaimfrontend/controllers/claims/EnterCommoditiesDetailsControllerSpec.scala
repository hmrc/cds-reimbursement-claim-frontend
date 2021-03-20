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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec

class EnterCommoditiesDetailsControllerSpec extends ControllerSpec {

  "Form Validation" must {
    val form             = EnterCommoditiesDetailsController.commoditiesDetailsForm
    val commodityDetails = "enter-commodities-details"
    val goodData         = Map(
      commodityDetails -> "A box of biscuits"
    )

    "accept good commodity details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "commodity details" should {
      "Accept longest possible details" in {
        val errors = form.bind(goodData.updated(commodityDetails, List.fill(500)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject details when it's too long" in {
        val errors = form.bind(goodData.updated(commodityDetails, List.fill(501)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }
  }
}
