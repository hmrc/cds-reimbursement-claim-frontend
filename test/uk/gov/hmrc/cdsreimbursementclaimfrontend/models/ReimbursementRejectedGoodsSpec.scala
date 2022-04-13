package uk.gov.hmrc.cdsreimbursementclaimfrontend.models


import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReimbursementRejectedGoodsSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  "The reimbursement claim" should {
    "be valid" when {
      "paid amount is greater than claim amount" in forAll(Gen.posNum[Int], Gen.posNum[Int]) { (amount, n) =>
        ReimbursementRejectedGoods(paidAmount = amount + n, claimAmount = amount).isValid should be(true)
      }

      "paid amount is equal to the claim amount" in forAll(Gen.posNum[Int], Gen.posNum[Int]) { (amount, n) =>
        ReimbursementRejectedGoods(paidAmount = amount, claimAmount = amount).isValid should be(true)
      }

    }

    "be invalid" when {
      "paid amount is lower than the claim amount" in forAll(Gen.posNum[Int], Gen.chooseNum(0, 2)) {
        (amount, n) =>
          ReimbursementRejectedGoods(paidAmount = amount, claimAmount = amount + n).isValid should be(false)
      }
    }

    "be unclaimed" in {
      ReimbursementRejectedGoods.unclaimed.isUnclaimed should be(true)
    }

    "have refund total as the claim amount" in forAll {
      (paidAmount: BigDecimal, claimAmount: BigDecimal) =>
        ReimbursementRejectedGoods(paidAmount, claimAmount).refundTotal should be(claimAmount)
    }

  }
}
