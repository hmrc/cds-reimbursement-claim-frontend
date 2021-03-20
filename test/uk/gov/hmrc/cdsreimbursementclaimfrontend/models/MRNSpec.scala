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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN._

class MRNSpec extends AnyWordSpec with Matchers {

  "MRN validation" should {

    "be valid on test MRNs" in {
      val mrns = List(
        "57WAFResponse11111",
        "57TimeoutResponse1",
        "57CouldNotProcess1",
        "57NoDeclarationF11",
        "57NoSecurityDepos1",
        "57EmptyResponse111",
        "57MinimumResponse1"
      )
      mrns.foreach(isValid(_) shouldBe true)
    }

    "fail if first 2 is not a number" in {
      isValid("aaWAFResponse11111") shouldBe false
    }

    "fail if chars 3-4 are numbers" in {
      isValid("1122FResponse11111") shouldBe false
    }

    "fail if the last char is a letter" in {
      isValid("11GBFResponse1111a") shouldBe false
    }

    "fail if not 18 characters" in {
      isValid("11GBResponse11111") shouldBe false
    }

    "Succeed with lower case letters" in {
      isValid("10ABCDEFGHIJKLMnO0") shouldBe true
    }

  }
}
