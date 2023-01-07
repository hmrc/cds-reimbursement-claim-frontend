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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._

class DisplayDeclarationSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks with JourneyGenerators {

  "The display declaration" should {
    "calculate the vat paid" in forAll(
      displayDeclarationGen,
      Gen.oneOf(TaxCodes.vatTaxCodes),
      Gen.listOfN(5, Gen.oneOf(TaxCodes.all).filterNot(TaxCodes.vatTaxCodes.contains(_))),
      Gen.choose(100, 10000),
      Gen.listOfN(5, Gen.choose(100, 10000))
    ) { (initialDisplayDeclaration, vatCode, nonVatCode, vatPaidAmount, nonVatPaidAmount) =>
      val vatNdrc    = NdrcDetails(vatCode.value, vatPaidAmount.toString, "payment-method", "payment-reference", None)
      val nonVatNdrc = nonVatCode.zip(nonVatPaidAmount).map { case (code, amount) =>
        NdrcDetails(code.value, amount.toString, "payment-method", "payment-reference", None)
      }

      val drd                = initialDisplayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(vatNdrc :: nonVatNdrc))
      val displayDeclaration = DisplayDeclaration(drd)

      displayDeclaration.totalVatPaidCharges.toString() shouldBe vatPaidAmount.toDouble.toString
    }

    "calculate the duty paid" in forAll(
      displayDeclarationGen,
      Gen.oneOf(TaxCodes.vatTaxCodes),
      Gen.listOfN(5, Gen.oneOf(TaxCodes.all).filterNot(TaxCodes.vatTaxCodes.contains(_))),
      Gen.choose(100, 10000),
      Gen.listOfN(5, Gen.choose(100, 10000))
    ) { (initialDisplayDeclaration, vatCode, nonVatCode, vatPaidAmount, nonVatPaidAmount) =>
      val vatNdrc    = NdrcDetails(vatCode.value, vatPaidAmount.toString, "payment-method", "payment-reference", None)
      val nonVatNdrc = nonVatCode.zip(nonVatPaidAmount).map { case (code, amount) =>
        NdrcDetails(code.value, amount.toString, "payment-method", "payment-reference", None)
      }

      val drd                = initialDisplayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(vatNdrc :: nonVatNdrc))
      val displayDeclaration = DisplayDeclaration(drd)

      displayDeclaration.totalDutiesPaidCharges.toString() shouldBe nonVatPaidAmount.sum.toDouble.toString
    }
  }
}
