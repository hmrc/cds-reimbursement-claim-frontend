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

class EnterClaimantDetailsAsIndividualControllerSpec extends ControllerSpec {

  "Form Validation" must {
    val form              = EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
    val fullName          = "enter-claimant-details-individual.individual-full-name"
    val emailAddress      = "enter-claimant-details-individual.individual-email"
    val phone             = "enter-claimant-details-individual.individual-phone-number"
    val addressLine1      = "nonUkAddress-line1"
    val addressLine2      = "nonUkAddress-line2"
    val addressLine3      = "nonUkAddress-line3"
    val addressLine4      = "nonUkAddress-line4"
    val postCode          = "postcode"
    val countryCode       = "countryCode"
    val addCompanyDetails = "enter-claimant-details-individual.add-company-details"

    val goodData = Map(
      fullName          -> "Magnus Magnusson",
      emailAddress      -> "mangus@email.com",
      phone             -> "0155555555",
      addressLine1      -> "57 Jex Belaran",
      addressLine2      -> "Eisraim Road",
      addressLine3      -> "",
      addressLine4      -> "Coventry",
      postCode          -> "CV3 6EA",
      countryCode       -> "GB",
      addCompanyDetails -> "false"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "Name" should {
      "Accept longest possible names" in {
        val errors = form.bind(goodData.updated(fullName, List.fill(512)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject names too long" in {
        val errors = form.bind(goodData.updated(fullName, List.fill(513)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Email" should {
      "Accept longest possible email" in {
        val email  = List.fill(233)("a").mkString("") + "@abc.com" //Allthogether 124
        val errors = form.bind(goodData.updated(emailAddress, email)).errors
        errors shouldBe Nil
      }
      "Reject email too long" in {
        val email  = List.fill(234)("a").mkString("") + "@abc.com" //Allthogether 125
        val errors = form.bind(goodData.updated(emailAddress, email)).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Phone" should {
      "Accept longest possible number" in {
        val errors = form.bind(goodData.updated(phone, List.fill(30)("1").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject numbers too long" in {
        val errors = form.bind(goodData.updated(phone, List.fill(31)("1").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Address Line 1" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine1, List.fill(35)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine1, List.fill(36)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Address Line 2" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine2, List.fill(35)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine2, List.fill(36)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Address Line 3" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine3, List.fill(35)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine3, List.fill(36)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Address Line 4" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine4, List.fill(35)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine4, List.fill(36)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Postcode" should {
      "Accept longest possible post code" in {
        val errors = form.bind(goodData.updated(postCode, "BD17 7DG")).errors
        errors shouldBe Nil
      }
      "Reject post code too long after trim" in {
        val errors = form.bind(goodData.updated(postCode, "BDD17 7DG")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
      "Reject invalid post code" in {
        val errors = form.bind(goodData.updated(postCode, "BDD7 7DG")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.pattern")
      }
      "Reject post code if too long" in {
        val errors = form.bind(goodData.updated(postCode, "BD17 7DGGA")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
      "Accept Anything outside of the UK" in {
        val errors = form.bind(goodData.updated(countryCode, "IS").updated(postCode, "AA2")).errors
        errors shouldBe Nil
      }
      "Reject Anything outside of the UK if it's too long" in {
        val errors = form.bind(goodData.updated(countryCode, "IS").updated(postCode, "1234567890")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Country Code" ignore { //TODO Ignored
      "Accept 2 digits country code" in { //TODO fix the getClass.getResourceAsStream in Country.scala
        val errors = form.bind(goodData.updated(countryCode, "HU")).errors
        errors shouldBe Nil
      }

      "Reject 1 digit" in { //TODO fix the getClass.getResourceAsStream in Country.scala
        val errors = form.bind(goodData.updated(countryCode, "H")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.notFound")
      }

      "Reject 3 digits" in { //TODO fix the getClass.getResourceAsStream in Country.scala
        val errors = form.bind(goodData.updated(countryCode, "HUH")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.notFound")
      }
    }

    "Add Company Details" should {
      "accept true" in {
        val errors = form.bind(goodData.updated(addCompanyDetails, "true")).errors
        errors shouldBe Nil
      }
      "accept false" in {
        val errors = form.bind(goodData.updated(addCompanyDetails, "true")).errors
        errors shouldBe Nil
      }
      "accept reject anything else" in {
        val errors = form.bind(goodData.updated(addCompanyDetails, "truea")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.boolean")
      }
    }

  }
}
