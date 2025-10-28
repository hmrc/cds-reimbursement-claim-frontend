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
import play.api.data.Form
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterInspectionDateForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class FormsSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  "Eori Form" should {
    val eoriNumber       = "enter-declarant-eori-number"
    val form: Form[Eori] = Forms.eoriNumberForm(eoriNumber)

    "Accept a valid Eori" in forAll { (eori: Eori) =>
      val errors = form.bind(Map(eoriNumber -> eori.value)).errors
      errors shouldBe Nil
    }

    "Accept shortest possible Eori" in forAll(Gen.listOfN(12, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB${ending.mkString}")).errors
      errors shouldBe Nil
    }

    "Accept longest possible Eori" in forAll(Gen.listOfN(14, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB1${ending.mkString}")).errors
      errors shouldBe Nil
    }

    "Reject too long Eori" in forAll(Gen.listOfN(14, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB?${ending.mkString}")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
    }

    "Reject way too long Eori" in forAll(Gen.listOfN(17, Gen.numChar)) { ending =>
      val errors = form.bind(Map(eoriNumber -> s"GB${ending.mkString}")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
    }
  }

  "MRN Form" should {
    val mrnKey          = "enter-movement-reference-number"
    val form: Form[MRN] = Forms.movementReferenceNumberForm

    "Accept a valid MRN" in forAll(genMRN) { mrn =>
      val errors = form.bind(Map(mrnKey -> mrn.value)).errors
      errors shouldBe Nil
    }

    "Reject empty MRN" in {
      val errors = form.bind(Map(mrnKey -> "")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
    }

    "Reject too long MRN" in forAll(Gen.listOfN(19, Gen.alphaNumChar)) { mrnChars =>
      val errors = form.bind(Map(mrnKey -> mrnChars.toString())).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.length")
    }

    "Reject too short MRN" in forAll(Gen.listOfN(17, Gen.alphaNumChar)) { mrnChars =>
      val errors = form.bind(Map(mrnKey -> mrnChars.toString())).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.length")
    }

    "Reject MRN containing special characters" in {
      val errors = form.bind(Map(mrnKey -> "01AAAAAA*AAAAAAAA2")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.characters")
    }

    "Reject MRN in the wrong format" in {
      val errors = form.bind(Map(mrnKey -> "12AAAAAAAAAAAAAAAB")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.format")
    }
  }

  "Export MRN Forms" when {
    val mrnKey                                 = "enter-export-movement-reference-number"
    val firstExportMrnForm: Form[(MRN, YesNo)] = Forms.firstExportMovementReferenceNumberForm
    val firstYesNoKey                          = s"$mrnKey.securities.yes-no"

    "First export MRN" should {
      "Accept a valid MRN" in forAll(genMRN) { mrn =>
        val errors = firstExportMrnForm.bind(Map(mrnKey -> mrn.value, firstYesNoKey -> "true")).errors
        errors shouldBe Nil
      }

      "Accept a valid CHIEF entry" in forAll(genChiefEntryMRN) { mrn =>
        val errors = firstExportMrnForm.bind(Map(mrnKey -> mrn.value, firstYesNoKey -> "true")).errors
        errors shouldBe Nil
      }

      "Reject empty MRN" in {
        val errors = firstExportMrnForm.bind(Map(mrnKey -> "", firstYesNoKey -> "true")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
      }

      "Reject empty yes no option" in {
        val errors = firstExportMrnForm.bind(Map(mrnKey -> genMRN.sample.get.value, firstYesNoKey -> "")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
      }

      "Reject MRN containing special characters" in {
        val errors = firstExportMrnForm.bind(Map(mrnKey -> "01AAAAAA*AAAAAAAA2", firstYesNoKey -> "true")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.characters")
      }

      "Reject MRN in the wrong format" in {
        val errors = firstExportMrnForm.bind(Map(mrnKey -> "12AAAAAAAAAAAAAAAB", firstYesNoKey -> "true")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.format")
      }
    }

    "Next export MRN" should {
      val nextMrnKey                                    = "enter-export-movement-reference-number.next"
      val nextExportMrnForm: Form[(MRN, Option[YesNo])] = Forms.nextExportMovementReferenceNumberForm
      val nextYesNoKey                                  = s"$nextMrnKey.securities.yes-no"

      "Accept a valid MRN" in forAll(genMRN) { mrn =>
        val errors = nextExportMrnForm.bind(Map(nextMrnKey -> mrn.value, nextYesNoKey -> "true")).errors
        errors shouldBe Nil
      }

      "Accept a valid CHIEF entry" in forAll(genChiefEntryMRN) { mrn =>
        val errors = nextExportMrnForm.bind(Map(nextMrnKey -> mrn.value, nextYesNoKey -> "true")).errors
        errors shouldBe Nil
      }

      "Reject empty MRN" in {
        val errors = nextExportMrnForm.bind(Map(nextMrnKey -> "", nextYesNoKey -> "true")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
      }

      "Accept empty optional yes no option" in {
        val errors = nextExportMrnForm.bind(Map(nextMrnKey -> genMRN.sample.get.value, nextYesNoKey -> "")).errors
        errors shouldBe Nil
      }

      "Reject MRN containing special characters" in {
        val errors = nextExportMrnForm.bind(Map(nextMrnKey -> "01AAAAAA*AAAAAAAA2", nextYesNoKey -> "true")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.characters")
      }

      "Reject MRN in the wrong format" in {
        val errors = nextExportMrnForm.bind(Map(nextMrnKey -> "12AAAAAAAAAAAAAAAB", nextYesNoKey -> "true")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.format")
      }
    }
  }

  "Duplicate MRN Form" should {
    val mrnKey          = "enter-duplicate-movement-reference-number"
    val mainMrn         = MRN("01AAAAAAAAAAAAAAA2")
    val form: Form[MRN] = Forms.enterDuplicateMrnCheckingAgainst(mainMrn)

    "Accept a valid MRN" in forAll(genMRN) { mrn =>
      val errors = form.bind(Map(mrnKey -> mrn.value)).errors
      errors shouldBe Nil
    }

    "Reject empty MRN" in {
      val errors = form.bind(Map(mrnKey -> "")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
    }

    "Reject too long MRN" in forAll(Gen.listOfN(19, Gen.alphaNumChar)) { mrnChars =>
      val errors = form.bind(Map(mrnKey -> mrnChars.toString())).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.length")
    }

    "Reject too short MRN" in forAll(Gen.listOfN(17, Gen.alphaNumChar)) { mrnChars =>
      val errors = form.bind(Map(mrnKey -> mrnChars.toString())).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.length")
    }

    "Reject MRN containing special characters" in {
      val errors = form.bind(Map(mrnKey -> "01AAAAAA*AAAAAAAA2")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.characters")
    }

    "Reject MRN in the wrong format" in {
      val errors = form.bind(Map(mrnKey -> "12AAAAAAAAAAAAAAAB")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.format")
    }

    "Reject MRN matching main MRN" in {
      val errors = form.bind(Map(mrnKey -> mainMrn.value)).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.enter-different-mrn")
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
