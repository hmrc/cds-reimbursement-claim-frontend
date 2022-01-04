/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReimbursementSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  "The reimbursement claim" should {
    "be valid" when {
      "paid amount is greater than should paid of amount" in forAll(Gen.posNum[Int], Gen.posNum[Int]) { (amount, n) =>
        Reimbursement(paidAmount = amount + n, shouldOfPaid = amount).isValid should be(true)
      }
    }

    "be invalid" when {
      "paid amount is lower or equal to should paid of amount" in forAll(Gen.posNum[Int], Gen.chooseNum(0, 2)) {
        (amount, n) =>
          Reimbursement(paidAmount = amount, shouldOfPaid = amount + n).isValid should be(false)
      }
    }

    "be unclaimed" in {
      Reimbursement.unclaimed.isUnclaimed should be(true)
    }

    "have refund total as subtraction of should paid amount from paid amount" in forAll {
      (paidAmount: BigDecimal, shouldPaidAmount: BigDecimal) =>
        Reimbursement(paidAmount, shouldPaidAmount).refundTotal should be(paidAmount - shouldPaidAmount)
    }
  }
}
