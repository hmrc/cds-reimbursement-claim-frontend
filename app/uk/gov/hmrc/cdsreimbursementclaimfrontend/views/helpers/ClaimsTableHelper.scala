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

import cats.syntax.eq.*
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.EuDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.UkDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReclaimWithAmounts
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementWithCorrectAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.HeadCell
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow

import java.util.Locale
import scala.collection.immutable.SortedMap

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
    claims.map { case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, _, _) =>
      makeCommonRowCells(taxCode, claimAmount, paidAmount, taxCode.value) ++ Seq(
        TableRow(
          content = HtmlContent(
            messages("check-claim.table.change-link", claimAction(taxCode).url, s"change-link-$taxCode")
          ),
          attributes = Map("id" -> s"change-$taxCode"),
          classes = "govuk-link"
        )
      )
    }

  def claimsRowsForMultiple(
    claims: Seq[ReimbursementWithCorrectAmount],
    mrn: MRN,
    index: Int,
    claimAction: (Int, TaxCode) => Call
  )(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, _, _) =>
      val suffix = s"${mrn.value}-$taxCode"
      makeCommonRowCells(taxCode, claimAmount, paidAmount, suffix) ++ Seq(
        TableRow(
          content = HtmlContent(
            messages("check-claim.table.change-link", claimAction(index, taxCode).url, s"change-link-$suffix")
          ),
          attributes = Map("id" -> s"change-$suffix"),
          classes = "govuk-link"
        )
      )
    } ++ Seq(
      makeTotalRowCells(
        claims.map(_.amount).sum,
        claims.map(_.paidAmount).sum,
        s"${mrn.value}"
      )
    )

  def claimsRowsForScheduled(
    claims: Seq[ReimbursementWithCorrectAmount],
    mainDutyType: String,
    claimAction: (DutyType, TaxCode) => Call
  )(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, _, Some(dutyType)) =>
      val suffix = s"$dutyType-$taxCode"
      makeCommonRowCells(taxCode, claimAmount, paidAmount, suffix) ++ Seq(
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
        s"$mainDutyType"
      )
    )

  private def makeTotalRowCells(
    claimAmountTotal: BigDecimal,
    paidAmountTotal: BigDecimal,
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
        classes = "govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmount.toPoundSterlingString),
        attributes = Map("id" -> s"claim-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      )
    )

  def claimsTotalSummary(claims: Seq[ReimbursementWithCorrectAmount])(implicit
    messages: Messages
  ): Seq[SummaryListRow] = claimsTotalSummary(claims.map(_.amount).sum)

  def claimsTotalSummary(total: BigDecimal)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.table.total")),
          classes = "govuk-heading-m govuk-!-padding-bottom-3"
        ),
        value = Value(
          content = Text(total.toPoundSterlingString),
          classes = "govuk-!-margin-bottom-3 govuk-heading-m"
        ),
        classes = "govuk-summary-list govuk-!-margin-bottom-3 govuk-heading-l  govuk-summary-list--no-border"
      )
    )

  def makeSecurityClaimFullAmount(
    securityDepositId: String,
    reclaims: List[ReclaimWithAmounts],
    declaration: DisplayDeclaration,
    key: String,
    fullAmountChangeCallOpt: Option[String => Call]
  )(implicit messages: Messages): SummaryList = SummaryList(
    attributes = Map("id" -> s"claim-full-amount-$securityDepositId"),
    classes = "govuk-summary-list--no-border",
    rows = Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages("check-claim.securities.claiming-full-amount"))),
        value = Value(
          Text(
            messages(
              if declaration.isFullSecurityAmount(securityDepositId, reclaims.map(_.claimAmount).sum) then
                s"$key.claim-full-amount.yes"
              else s"$key.claim-full-amount.no"
            )
          )
        ),
        actions = fullAmountChangeCallOpt.map(fullAmountChangeCall =>
          Actions(
            items = Seq(
              ActionItem(
                attributes = Map("id" -> s"change-claim-full-amount-$securityDepositId"),
                href = fullAmountChangeCall(securityDepositId).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages(
                    "check-claim.securities.hidden.claim-full-amount",
                    messages(s"$key.claim-full-amount.label").toLowerCase(Locale.ROOT),
                    securityDepositId
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def claimsRowsForSecurities(
    securityDepositId: String,
    claims: Seq[ReclaimWithAmounts],
    claimAction: (String, TaxCode) => Call
  )(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReclaimWithAmounts(taxCode, claimAmount, paidAmount) =>
      val suffix = s"$securityDepositId-$taxCode"
      makeCommonRowCells(taxCode, claimAmount, paidAmount, suffix) ++ Seq(
        TableRow(
          content = HtmlContent(
            messages(
              "check-claim.table.change-link",
              claimAction(securityDepositId, taxCode).url,
              s"change-link-$suffix"
            )
          ),
          attributes = Map("id" -> s"change-$suffix"),
          classes = "govuk-link govuk-table__cell--numeric"
        )
      )
    } ++ Seq(
      makeTotalRowCells(
        claims.map(_.claimAmount).sum,
        claims.map(_.paidAmount).sum,
        securityDepositId
      )
    )

  def sortReimbursementsByDisplayDuty(
    reimbursements: SortedMap[DutyType, List[ReimbursementWithCorrectAmount]]
  ): Map[String, List[ReimbursementWithCorrectAmount]] = {
    val ukDuties     = reimbursements.filter(_._1 === UkDuty).values.flatten.toList
    val euDuties     = reimbursements.filter(_._1 === EuDuty).values.flatten.toList
    val exciseDuties = reimbursements.filterNot(i => i._1 === UkDuty || i._1 === EuDuty).values.flatten.toList

    Map(
      UkDuty.repr   -> ukDuties,
      EuDuty.repr   -> euDuties,
      "excise-duty" -> exciseDuties
    ).filterNot(_._2.isEmpty)
  }

}
