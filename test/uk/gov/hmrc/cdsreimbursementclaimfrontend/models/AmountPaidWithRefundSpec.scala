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

class AmountPaidWithRefundSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  "The reimbursement claim" should {
    "be valid" when {
      "paid amount is greater than claim amount" in forAll(Gen.posNum[Int], Gen.chooseNum(1, 2)) { (amount, n) =>
        AmountPaidWithRefund(paidAmount = amount + n, refundAmount = amount).isValid should be(true)
      }

      "paid amount is equal to the claim amount" in forAll(Gen.posNum[Int], Gen.posNum[Int]) { (amount, _) =>
        AmountPaidWithRefund(paidAmount = amount, refundAmount = amount).isValid should be(true)
      }

    }

    "be invalid" when {
      "paid amount is lower than the claim amount" in forAll(Gen.chooseNum(10, 20), Gen.chooseNum(1, 2)) { (amount, n) =>
        AmountPaidWithRefund(paidAmount = amount, refundAmount = amount + n).isValid should be(false)
      }
    }

    "be unclaimed" in {
      AmountPaidWithRefund.unclaimed.isUnclaimed should be(true)
    }

    "have refund total as the claim amount" in forAll { (paidAmount: BigDecimal, claimAmount: BigDecimal) =>
      AmountPaidWithRefund(paidAmount, claimAmount).refundAmount should be(claimAmount)
    }

  }
}
