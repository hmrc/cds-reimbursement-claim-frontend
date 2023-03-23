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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterInspectionDateForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

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

  "Enter Inspection Date Form" must {
    val form = enterInspectionDateForm

    val dateOfInspectionDay   = "enter-inspection-date.rejected-goods.day"
    val dateOfInspectionMonth = "enter-inspection-date.rejected-goods.month"
    val dateOfInspectionYear  = "enter-inspection-date.rejected-goods.year"

    val goodData = Map(
      dateOfInspectionDay   -> "20",
      dateOfInspectionMonth -> "3",
      dateOfInspectionYear  -> "1987"
    )

    "accept a valid date" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "Day of Inspection" should {
      "Reject days too big" in {
        val errors = form.bind(goodData.updated(dateOfInspectionDay, "32")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject days too small" in {
        val errors = form.bind(goodData.updated(dateOfInspectionDay, "0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject valid days in 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfInspectionDay, "015")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject days with chars" in {
        val errors = form.bind(goodData.updated(dateOfInspectionDay, "Ab")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }
    }

    "Month of Inspection" should {
      "Reject months too big" in {
        val errors = form.bind(goodData.updated(dateOfInspectionMonth, "13")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject months too small" in {
        val errors = form.bind(goodData.updated(dateOfInspectionMonth, "0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject valid months in 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfInspectionMonth, "012")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject months with chars" in {
        val errors = form.bind(goodData.updated(dateOfInspectionMonth, "Ja")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

    }

    "Year of Inspection" should {
      "Reject years too early" in {
        val errors = form.bind(goodData.updated(dateOfInspectionYear, "1899")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.before1900")
      }

      "Reject 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfInspectionYear, "202")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject years with chars" in {
        val errors = form.bind(goodData.updated(dateOfInspectionYear, "202a")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

    }
  }
}
