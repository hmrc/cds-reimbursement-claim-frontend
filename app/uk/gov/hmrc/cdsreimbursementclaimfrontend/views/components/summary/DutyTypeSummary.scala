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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer

sealed abstract class DutyTypeSummary(val total: BigDecimal, val messageKey: String)

object DutyTypeSummary {

  final case class UKDutyTypeSummary(override val total: BigDecimal)
      extends DutyTypeSummary(total, messageKey = "uk-duty.label")

  final case class EUDutyTypeSummary(override val total: BigDecimal)
      extends DutyTypeSummary(total, messageKey = "eu-duty.label")

  final case class ExciseDutyTypeSummary(override val total: BigDecimal)
      extends DutyTypeSummary(total, messageKey = "excise-duty.label")

  def buildFrom(reimbursements: ClaimedReimbursementsAnswer): Seq[DutyTypeSummary] = {
    val totals = reimbursements.foldLeft(Array[BigDecimal](xs = 0, 0, 0))((buff, reimbursement) =>
      if (TaxCodes.UK.contains(reimbursement.taxCode)) {
        buff(0) += reimbursement.claimAmount
        buff
      } else if (TaxCodes.EU.contains(reimbursement.taxCode)) {
        buff(1) += reimbursement.claimAmount
        buff
      } else if (TaxCodes.excise.contains(reimbursement.taxCode)) {
        buff(2) += reimbursement.claimAmount
        buff
      } else buff
    )

    Seq[DutyTypeSummary](
      UKDutyTypeSummary(totals(0)),
      EUDutyTypeSummary(totals(1)),
      ExciseDutyTypeSummary(totals(2))
    ).filter(_.total > 0)
  }
}
