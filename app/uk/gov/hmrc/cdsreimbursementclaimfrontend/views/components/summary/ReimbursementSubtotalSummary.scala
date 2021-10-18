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

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyType, ReimbursementClaim}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementClaimAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MoneyUtils
import uk.gov.hmrc.govukfrontend.views.Aliases.{Key, Value}
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ActionItem, Actions, SummaryList, SummaryListRow}

class ReimbursementSubtotalSummary extends AnswerSummary[(DutyType, Map[TaxCode, ReimbursementClaim])] {

  def render(key: String, answer: (DutyType, Map[TaxCode, ReimbursementClaim]))(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList = {
    val dutyType       = answer._1
    val reimbursements = answer._2

    SummaryList(
      reimbursements.map { taxCodeWithClaim: (TaxCode, ReimbursementClaim) =>
        val taxCode       = taxCodeWithClaim._1
        val reimbursement = taxCodeWithClaim._2

        SummaryListRow(
          key = Key(Text(messages(s"$key.duty-code.row.key", messages(s"tax-code.${taxCode.value}")))),
          value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(reimbursement.refundTotal))),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterReimbursementClaimController.showReimbursementClaim(dutyType, taxCode).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.duty-code.row.key", messages(s"tax-code.${taxCode.value}")))
                )
              )
            )
          )
        )
      }.toSeq ++ (
        if (reimbursements.size > 1) {
          Seq(
            SummaryListRow(
              key = Key(Text(messages(s"$key.duty-code.total.key", messages(s"duty-type.${dutyType.repr}")))),
              value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(reimbursements.subtotal)))
            )
          )
        } else Nil
      )
    )
  }
}
