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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.data.NonEmptyList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, Reimbursement, TaxCode}

object ClaimsAnswer {

  def apply(head: Claim, tail: Claim*): ClaimsAnswer =
    NonEmptyList.of(head, tail: _*)

  def apply(l: List[Claim]): Option[ClaimsAnswer] =
    NonEmptyList.fromList(l)

  def apply(selectedDutyTaxCodesReimbursements: SelectedDutyTaxCodesReimbursementAnswer): Option[ClaimsAnswer] = {

    def toClaim(taxCodeWithClaim: (TaxCode, Reimbursement)) =
      Claim(
        taxCode = taxCodeWithClaim._1,
        paidAmount = taxCodeWithClaim._2.paidAmount,
        claimAmount = taxCodeWithClaim._2.shouldOfPaid
      )

    selectedDutyTaxCodesReimbursements.combine.flatMap { combinedReimbursements =>
      NonEmptyList.fromList(combinedReimbursements.map(toClaim).toList)
    }
  }
}
