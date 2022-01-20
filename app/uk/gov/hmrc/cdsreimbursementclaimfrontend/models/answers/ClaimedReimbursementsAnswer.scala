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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.Id
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxOption
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

object ClaimedReimbursementsAnswer {

  def apply(head: ClaimedReimbursement, tail: ClaimedReimbursement*): ClaimedReimbursementsAnswer =
    NonEmptyList.of(head, tail: _*)

  def apply(items: List[ClaimedReimbursement]): Option[ClaimedReimbursementsAnswer] =
    NonEmptyList.fromList(items)

  def apply(reimbursements: SelectedDutyTaxCodesReimbursementAnswer): Option[ClaimedReimbursementsAnswer] = {

    def toClaim(taxCodeWithClaim: (TaxCode, Reimbursement)) =
      ClaimedReimbursement(
        taxCode = taxCodeWithClaim._1,
        paidAmount = taxCodeWithClaim._2.paidAmount,
        claimAmount = taxCodeWithClaim._2.refundTotal
      )

    reimbursements.combine.flatMap { combinedReimbursements =>
      NonEmptyList.fromList(combinedReimbursements.map(toClaim).toList)
    }
  }

  val validator: Validator[Id, ClaimedReimbursementsAnswer] = maybeClaimedReimbursements =>
    maybeClaimedReimbursements.toValidNel(MissingAnswerError("Claimed reimbursements"))

  implicit class ClaimedReimbursementsOps(val claims: NonEmptyList[ClaimedReimbursement]) extends AnyVal {
    def total: BigDecimal = claims.map(_.claimAmount).toList.sum
  }
}
