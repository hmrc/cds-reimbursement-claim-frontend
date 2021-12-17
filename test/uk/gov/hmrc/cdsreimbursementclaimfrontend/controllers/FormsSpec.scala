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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import org.scalacheck.Gen

class FormsSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  "Eori Form" should {
    val eoriNumber = "enter-declarant-eori-number"
    val form       = Forms.eoriNumberForm(eoriNumber)

    "Accept a valid Eori" in forAll { (eori: Eori) =>
      val errors = form.bind(Map(eoriNumber -> eori.value)).errors
      errors shouldBe Nil
    }

    "Accept shortest possible Eori" in forAll(Gen.listOfN(12, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB${ending.mkString}")).errors
      errors shouldBe Nil
    }

    "Accept longest possible Eori" in forAll(Gen.listOfN(15, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB${ending.mkString}")).errors
      errors shouldBe Nil
    }

    "Reject too long Eori" in forAll(Gen.listOfN(16, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB${ending.mkString}")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
    }

    "Reject way too long Eori" in forAll(Gen.listOfN(17, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB${ending.mkString}")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
    }
  }
}
