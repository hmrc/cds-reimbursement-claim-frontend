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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

class ImportDeclarationSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks with ClaimGenerators {

  "The display declaration" should {
    "calculate the vat paid" in forAll(
      importDeclarationGen,
      Gen.oneOf(TaxCodes.vatTaxCodes),
      Gen.listOfN(5, Gen.oneOf(TaxCodes.all).filterNot(_.isVAT)),
      Gen.choose(100, 10000),
      Gen.listOfN(5, Gen.choose(100, 10000))
    ) { (initialImportDeclaration, vatCode, nonVatCode, vatPaidAmount, nonVatPaidAmount) =>
      val vatNdrc    = NdrcDetails(vatCode.value, vatPaidAmount.toString, "payment-method", "payment-reference", None)
      val nonVatNdrc = nonVatCode.zip(nonVatPaidAmount).map { case (code, amount) =>
        NdrcDetails(code.value, amount.toString, "payment-method", "payment-reference", None)
      }

      val drd               = initialImportDeclaration.displayResponseDetail.copy(ndrcDetails = Some(vatNdrc :: nonVatNdrc))
      val importDeclaration = ImportDeclaration(drd)

      importDeclaration.totalVatPaidCharges match {
        case None         => vatPaidAmount.toDouble.toString shouldBe "0.0"
        case Some(amount) => amount.toString()               shouldBe vatPaidAmount.toDouble.toString
      }
    }

    "calculate the duty paid" in forAll(
      importDeclarationGen,
      Gen.oneOf(TaxCodes.vatTaxCodes),
      Gen.listOfN(5, Gen.oneOf(TaxCodes.all).filterNot(_.isVAT)),
      Gen.choose(100, 10000),
      Gen.listOfN(5, Gen.choose(100, 10000))
    ) { (initialImportDeclaration, vatCode, nonVatCode, vatPaidAmount, nonVatPaidAmount) =>
      val vatNdrc    = NdrcDetails(vatCode.value, vatPaidAmount.toString, "payment-method", "payment-reference", None)
      val nonVatNdrc = nonVatCode.zip(nonVatPaidAmount).map { case (code, amount) =>
        NdrcDetails(code.value, amount.toString, "payment-method", "payment-reference", None)
      }

      val drd               = initialImportDeclaration.displayResponseDetail.copy(ndrcDetails = Some(vatNdrc :: nonVatNdrc))
      val importDeclaration = ImportDeclaration(drd)

      importDeclaration.totalDutiesPaidCharges.toString() shouldBe nonVatPaidAmount.sum.toDouble.toString
    }

    "check containsSomeUnsupportedTaxCode" in {
      exampleImportDeclaration.containsSomeUnsupportedTaxCode shouldBe false
      val declaration = exampleImportDeclarationWithSomeUnsupportedCode
      declaration.containsOnlyUnsupportedTaxCodes shouldBe false
      declaration.containsSomeUnsupportedTaxCode  shouldBe true

    }

    "check containsOnlyUnsupportedTaxCodes" in {
      exampleImportDeclaration.containsOnlyUnsupportedTaxCodes shouldBe false
      val declaration = exampleImportDeclarationWithOnlyUnsupportedCodes
      declaration.containsOnlyUnsupportedTaxCodes shouldBe true
      declaration.containsSomeUnsupportedTaxCode  shouldBe true
    }

    "removeUnsupportedTaxCodes" in {
      exampleImportDeclaration.removeUnsupportedTaxCodes()                                              shouldBe exampleImportDeclaration
      exampleImportDeclarationWithSomeUnsupportedCode.removeUnsupportedTaxCodes().getAvailableTaxCodes  shouldBe Seq(
        TaxCode.A00
      )
      exampleImportDeclarationWithOnlyUnsupportedCodes.removeUnsupportedTaxCodes().getAvailableTaxCodes shouldBe Seq()
    }
  }
}
