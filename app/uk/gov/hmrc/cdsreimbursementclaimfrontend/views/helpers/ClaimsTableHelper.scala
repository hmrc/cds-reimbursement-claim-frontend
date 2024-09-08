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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementWithCorrectAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.HeadCell
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow

object ClaimsTableHelper {
  def claimsTableHeaders(headerIdSuffix: String = "")(implicit
    messages: Messages
  ): Seq[HeadCell] =
    Seq(
      HeadCell(
        content = Text(messages("check-claim.table-header.selected-charges")),
        classes = "govuk-table__header",
        attributes = Map("id" -> s"selected-claim-header$headerIdSuffix")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.you-paid")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> s"you-paid-header$headerIdSuffix")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.should-have-paid")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> s"should-have-paid-header$headerIdSuffix")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.claim-amount")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> s"claim-amount-header$headerIdSuffix")
      ),
      HeadCell(
        content = Text(""),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> s"blank-header$headerIdSuffix")
      )
    )

  def claimsRowsForSingle(claims: Seq[ReimbursementWithCorrectAmount], claimAction: TaxCode => Call)(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, correctedAmount) =>
      makeCommonRowCells(taxCode, claimAmount, paidAmount, correctedAmount, taxCode.value) ++ Seq(
        TableRow(
          content = HtmlContent(
            messages("check-claim.table.change-link", claimAction(taxCode).url, s"change-link-$taxCode")
          ),
          attributes = Map("id" -> s"change-$taxCode"),
          classes = "govuk-link"
        )
      )
    }

  def claimsRowsForScheduled(
    claims: Seq[ReimbursementWithCorrectAmount],
    dutyType: DutyType,
    claimAction: (DutyType, TaxCode) => Call
  )(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, correctedAmount) =>
      val suffix = s"$dutyType-$taxCode"
      makeCommonRowCells(taxCode, claimAmount, paidAmount, correctedAmount, suffix) ++ Seq(
        TableRow(
          content = HtmlContent(
            messages("check-claim.table.change-link", claimAction(dutyType, taxCode).url, s"change-link-$suffix")
          ),
          attributes = Map("id" -> s"change-$suffix"),
          classes = "govuk-link"
        )
      )
    } ++ Seq(
      makeTotalRowCells(
        claims.map(_.amount).sum,
        claims.map(_.paidAmount).sum,
        claims.map(_.correctedAmount).sum,
        s"$dutyType"
      )
    )

  private def makeTotalRowCells(
    claimAmountTotal: BigDecimal,
    paidAmountTotal: BigDecimal,
    correctedAmountTotal: BigDecimal,
    idSuffix: String
  )(implicit messages: Messages): Seq[TableRow] =
    Seq(
      TableRow(
        content = HtmlContent(messages("check-claim.total.header")),
        attributes = Map("id" -> s"total-$idSuffix"),
        classes = "govuk-table__header"
      ),
      TableRow(
        content = Text(paidAmountTotal.toPoundSterlingString),
        attributes = Map("id" -> s"what-you-paid-total-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(correctedAmountTotal.toPoundSterlingString),
        attributes = Map("id" -> s"you-should-have-paid-total-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmountTotal.toPoundSterlingString),
        attributes = Map("id" -> s"claim-amount-total-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(""),
        attributes = Map("id" -> s"blank-cell-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      )
    )

  private def makeCommonRowCells(
    taxCode: TaxCode,
    claimAmount: BigDecimal,
    paidAmount: BigDecimal,
    correctedAmount: BigDecimal,
    idSuffix: String
  )(implicit messages: Messages): Seq[TableRow] =
    Seq(
      TableRow(
        content = HtmlContent(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}"),
        attributes = Map("id" -> s"selected-claim-$idSuffix"),
        classes = "govuk-table__header"
      ),
      TableRow(
        content = Text(paidAmount.toPoundSterlingString),
        attributes = Map("id" -> s"what-you-paid-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(correctedAmount.toPoundSterlingString),
        attributes = Map("id" -> s"you-should-have-paid-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmount.toPoundSterlingString),
        attributes = Map("id" -> s"claim-amount-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      )
    )

  def claimsTotalSummary(claims: Seq[ReimbursementWithCorrectAmount])(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
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
