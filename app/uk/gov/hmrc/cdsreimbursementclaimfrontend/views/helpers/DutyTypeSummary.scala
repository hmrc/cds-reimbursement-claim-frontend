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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

sealed abstract class DutyTypeSummary(val total: BigDecimal, val messageKey: String)

object DutyTypeSummary {

  final case class UKDutyTypeSummary(override val total: BigDecimal)
      extends DutyTypeSummary(total, messageKey = "uk-duty.label")

  final case class EUDutyTypeSummary(override val total: BigDecimal)
      extends DutyTypeSummary(total, messageKey = "eu-duty.label")

  final case class ExciseDutyTypeSummary(override val total: BigDecimal)
      extends DutyTypeSummary(total, messageKey = "excise-duty.label")

  def buildFrom(reimbursements: ClaimedReimbursementsAnswer): Seq[DutyTypeSummary] =
    buildFrom(reimbursements.toList.map(r => (r.taxCode, r.claimAmount)))

  def buildFrom(reimbursements: Seq[(TaxCode, BigDecimal)]): Seq[DutyTypeSummary] = {
    val totals = reimbursements
      .foldLeft(Array[BigDecimal](xs = 0, 0, 0)) { case (buff, (taxCode, claimAmount)) =>
        if (TaxCodes.ukTaxCodeSet.contains(taxCode)) {
          buff(0) += claimAmount
          buff
        } else if (TaxCodes.euTaxCodeSet.contains(taxCode)) {
          buff(1) += claimAmount
          buff
        } else if (TaxCodes.exciseTaxCodeSet.contains(taxCode)) {
          buff(2) += claimAmount
          buff
        } else buff
      }

    Seq[DutyTypeSummary](
      UKDutyTypeSummary(totals(0)),
      EUDutyTypeSummary(totals(1)),
      ExciseDutyTypeSummary(totals(2))
    ).filter(_.total > 0)
  }
}
