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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ClaimedReimbursement, TaxCode, TaxCodes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary.DutyTypeSummary._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary.DutyClaimSummarySpec.{genReimbursements, totalOf}

class DutyClaimSummarySpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  "The duty claim summary" should {
    "compute totals for different duties" in {
      forAll(genReimbursements(TaxCodes.UK), genReimbursements(TaxCodes.EU), genReimbursements(TaxCodes.excise)) {
        (ukReimbursements, euReimbursements, exciseReimbursements) =>
          val answer    = NonEmptyList.fromListUnsafe(ukReimbursements ++ euReimbursements ++ exciseReimbursements)
          val summaries = DutyTypeSummary.buildFrom(answer)

          summaries should contain allOf (
            UKDutyTypeSummary(totalOf(ukReimbursements)),
            EUDutyTypeSummary(totalOf(euReimbursements)),
            ExciseDutyTypeSummary(totalOf(exciseReimbursements))
          )
      }
    }

    "exclude empty summaries" in {
      forAll { taxCode: TaxCode =>
        DutyTypeSummary.buildFrom(
          NonEmptyList.one(
            ClaimedReimbursement(
              taxCode = taxCode,
              claimAmount = 0,
              paidAmount = 0
            )
          )
        ) should be(Nil)
      }
    }
  }
}

object DutyClaimSummarySpec {

  def genReimbursements(codes: Seq[TaxCode]): Gen[List[ClaimedReimbursement]] =
    for {
      n       <- Gen.choose(1, codes.length)
      picked  <- Gen.pick(n, codes)
      amounts <- Gen.listOfN(n, Gen.posNum[Double].map(BigDecimal(_)))
    } yield (picked zip amounts).map { case (taxCode, amount) =>
      ClaimedReimbursement(
        taxCode = taxCode,
        claimAmount = amount,
        paidAmount = 0
      )
    }.toList

  def totalOf(claims: List[ClaimedReimbursement]): BigDecimal =
    claims.map(_.claimAmount).sum
}
