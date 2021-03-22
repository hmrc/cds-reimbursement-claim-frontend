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

class EnterDeclarationDetailsControllerSpec extends ControllerSpec {

  "Form Validation" must {
    val form              = EnterDeclarationDetailsController.entryDeclarationDetailsForm
    val dateOfImportDay   = "enter-declaration-details.day"
    val dateOfImportMonth = "enter-declaration-details.month"
    val dateOfImportYear  = "enter-declaration-details.year"
    val placeOfImport     = "enter-declaration-details.place-of-import"
    val importerName      = "enter-declaration-details.importer-name"
    val importerEmail     = "enter-declaration-details.importer-email-address"
    val importerPhone     = "enter-declaration-details.importer-phone-number"
    val declarantName     = "enter-declaration-details.declarant-name"
    val declarantEmail    = "enter-declaration-details.declarant-email-address"
    val declarantPhone    = "enter-declaration-details.declarant-phone-number"

    val goodData = Map(
      dateOfImportDay   -> "20",
      dateOfImportMonth -> "3",
      dateOfImportYear  -> "1987",
      placeOfImport     -> "London",
      importerName      -> "John Johanson",
      importerEmail     -> "john@email.com",
      importerPhone     -> "01987032000",
      declarantName     -> "Magnus Magnusson",
      declarantEmail    -> "mangus@email.com",
      declarantPhone    -> "0155555555"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "Day of Import" should {
      "Reject days too big" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "32")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject days too small" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject valid days in 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "015")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Accept valid padded days" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "05")).errors
        errors shouldBe Nil
      }

      "Reject days with chars" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "Ab")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }
    }

    "Month of Import" should {
      "Reject months too big" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "13")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject months too small" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject valid months in 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "012")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Accept valid padded months" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "05")).errors
        errors shouldBe Nil
      }

      "Reject months with chars" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "Ja")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

    }

    "Year of Import" should {
      "Reject years too far" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "2120")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooFarInFuture")
      }

      "Reject years too early" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "1899")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.before1900")
      }

      "Reject 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "202")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject years with chars" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "202a")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

    }

    "Place of Import" should {
      "Accept longest possible names" in {
        val errors = form.bind(goodData.updated(placeOfImport, List.fill(70)("a").mkString(""))).errors
        errors shouldBe Nil
      }

      "Reject names too long" in {
        val errors = form.bind(goodData.updated(placeOfImport, List.fill(71)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Importer name" should {
      "Accept longest possible names" in {
        val errors = form.bind(goodData.updated(importerName, List.fill(160)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject names too long" in {
        val errors = form.bind(goodData.updated(importerName, List.fill(161)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Importer email" should {
      "Accept longest possible email" in {
        val email  = List.fill(233)("a").mkString("") + "@abc.com" //Allthogether 241
        val errors = form.bind(goodData.updated(importerEmail, email)).errors
        errors shouldBe Nil
      }
      "Reject email too long" in {
        val email  = List.fill(234)("a").mkString("") + "@abc.com" //Allthogether 242
        val errors = form.bind(goodData.updated(importerEmail, email)).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Importer phone" should {
      "Accept longest possible number" in {
        val errors = form.bind(goodData.updated(importerPhone, List.fill(30)("1").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject numbers too long" in {
        val errors = form.bind(goodData.updated(importerPhone, List.fill(31)("1").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }

      "Reject non-numbers" in {
        val errors = form.bind(goodData.updated(importerPhone, "123456789a")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid")
      }
    }

    "Declarant name" should {
      "Accept longest possible names" in {
        val errors = form.bind(goodData.updated(importerName, List.fill(160)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject names too long" in {
        val errors = form.bind(goodData.updated(importerName, List.fill(161)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Declarant email" should {
      "Accept longest possible email" in {
        val email  = List.fill(233)("a").mkString("") + "@abc.com" //Allthogether 241
        val errors = form.bind(goodData.updated(importerEmail, email)).errors
        errors shouldBe Nil
      }
      "Reject email too long" in {
        val email  = List.fill(234)("a").mkString("") + "@abc.com" //Allthogether 242
        val errors = form.bind(goodData.updated(importerEmail, email)).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Declarant phone" should {
      "Accept longest possible number" in {
        val errors = form.bind(goodData.updated(importerPhone, List.fill(30)("1").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject numbers too long" in {
        val errors = form.bind(goodData.updated(importerPhone, List.fill(31)("1").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }

      "Reject non-numbers" in {
        val errors = form.bind(goodData.updated(importerPhone, "123456789a")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid")
      }
    }
  }
}
