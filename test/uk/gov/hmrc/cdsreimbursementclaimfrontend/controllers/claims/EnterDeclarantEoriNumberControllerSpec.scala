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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.numStringGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.eoriGen

class EnterDeclarantEoriNumberControllerSpec extends ControllerSpec {

  "Entry Claim Amount Validation" must {
    val form       = EnterDeclarantEoriNumberController.eoriNumberForm
    val eoriNumber = "enter-declarant-eori-number"

    val goodData = Map(
      eoriNumber -> sample[Eori].value
    )

    "accept good declaration details" in {
      println(goodData)
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "Eori" should {
      "Accept shortedt possible Eori" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(12))).errors
        errors shouldBe Nil
      }
      "Accept longest possible Eori" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(15))).errors
        errors shouldBe Nil
      }
      "Reject too short Eoris" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(11))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }
      "Reject too long Eoris" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(16))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }
      "Reject way too long Eoris" in {
        val errors = form.bind(goodData.updated(eoriNumber, "GB" + numStringGen(17))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

  }

}
