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

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.Aliases.Key
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow

import scala.collection.immutable.SortedMap

object TaxCodeReimbursementRejectedGoodsSummary
    extends AnswerSummary[(DutyType, SortedMap[TaxCode, AmountPaidWithCorrect])] {

  def render(
    answer: (DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]),
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {
    val duty                      = answer._1
    val claimsMadeAgainstTaxCodes = answer._2

    SummaryList(
      claimsMadeAgainstTaxCodes.map { (taxCodeWithClaim: (TaxCode, AmountPaidWithCorrect)) =>
        val taxCode       = taxCodeWithClaim._1
        val reimbursement = taxCodeWithClaim._2

        SummaryListRow(
          key = Key(Text(messages(s"$key.duty-code.row.key", messages(s"tax-code.$taxCode")))),
          value = Value(Text(reimbursement.claimAmount.toPoundSterlingString)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = routes.EnterClaimController.show(duty, taxCode).url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.duty-code.row.key", messages(s"tax-code.$taxCode")))
                )
              )
            )
          )
        )
      }.toSeq ++ (
        if claimsMadeAgainstTaxCodes.size > 1 then {
          Seq(
            SummaryListRow(
              key = Key(Text(messages(s"$key.duty-code.total.key", messages(s"duty-type.${duty.repr}")))),
              value = Value(
                Text(
                  claimsMadeAgainstTaxCodes.values
                    .foldLeft(BigDecimal(0))((total, claim) => total + claim.claimAmount)
                    .toPoundSterlingString
                )
              )
            )
          )
        } else Nil
      )
    )
  }
}
