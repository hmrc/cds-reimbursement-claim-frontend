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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.HeadCell
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow

object ClaimsTableHelper {
  def claimsTableHeaders(implicit
    messages: Messages
  ): Seq[HeadCell] =
    Seq(
      HeadCell(
        content = Text(messages("check-claim.table-header.selected-charges")),
        classes = "govuk-table__header",
        attributes = Map("id" -> "selected-claim-header")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.you-paid")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "you-paid-header")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.should-have-paid")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "should-have-paid-header")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.claim-amount")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "claim-amount-header")
      ),
      HeadCell(
        content = Text(""),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "blank-header")
      )
    )

  def claimsRows(claims: Seq[Reimbursement], claimAction: TaxCode => Call)(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.zipWithIndex.map { case (Reimbursement(taxCode, claimAmount, _, paidAmount, Some(correctedAmount)), index) =>
      Seq(
        TableRow(
          content = HtmlContent(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}"),
          attributes = Map("id" -> s"selected-claim-${index + 1}"),
          classes = "govuk-table__header"
        ),
        TableRow(
          content = Text(paidAmount.toPoundSterlingString),
          attributes = Map("id" -> s"what-you-paid-${index + 1}"),
          classes = "govuk-table__cell govuk-table__cell--numeric"
        ),
        TableRow(
          content = Text(correctedAmount.toPoundSterlingString),
          attributes = Map("id" -> s"you-should-have-paid-${index + 1}"),
          classes = "govuk-table__cell govuk-table__cell--numeric"
        ),
        TableRow(
          content = Text(claimAmount.toPoundSterlingString),
          attributes = Map("id" -> s"claim-amount-${index + 1}"),
          classes = "govuk-table__cell govuk-table__cell--numeric"
        ),
        TableRow(
          content = HtmlContent(
            messages("check-claim.table.change-link", claimAction(taxCode).url, s"change-link-${index + 1}")
          ),
          attributes = Map("id" -> s"change-${index + 1}"),
          classes = "govuk-link"
        )
      )
    }

  def claimsTotalSummary(claims: Seq[Reimbursement])(implicit messages: Messages): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.table.total")),
          classes = "govuk-summary-list__key govuk-heading-m govuk-!-padding-bottom-3"
        ),
        value = Value(
          content = Text(claims.map(_.amount).sum.toPoundSterlingString),
          classes = "govuk-summary-list__value govuk-!-margin-bottom-3 govuk-heading-m govuk-summary-list__key"
        ),
        classes = "govuk-summary-list govuk-!-margin-bottom-3 govuk-heading-l  govuk-summary-list--no-border"
      )
    )

}
