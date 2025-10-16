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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReclaimWithAmounts
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementWithCorrectAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.HeadCell
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow

import java.util.Locale
import scala.collection.immutable.SortedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.Excise

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
        content = Text(messages("check-claim.table-header.full-amount")),
        classes = "govuk-table__header govuk-table__header--numeric govuk-table__header--nowrap",
        attributes = Map("id" -> s"full-amount-header$headerIdSuffix")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.claim-amount")),
        classes = "govuk-table__header govuk-table__header--numeric govuk-table__header--nowrap",
        attributes = Map("id" -> s"claim-amount-header$headerIdSuffix")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.action")),
        classes = "govuk-table__header govuk-table__header--numeric govuk-table__header--nowrap",
        attributes = Map("id" -> s"action-header$headerIdSuffix")
      )
    )

  def claimsTableHeadersSingleSecurity()(implicit
    messages: Messages
  ): Seq[HeadCell] =
    Seq(
      HeadCell(
        content = Text(messages("check-claim.table-header.selected-charges")),
        classes = "govuk-table__header",
        attributes = Map("id" -> "selected-claim-header")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.full-amount")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "full-amount-header")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.claim-amount")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "claim-amount-header")
      ),
      HeadCell(
        content = Text(messages("check-claim.table-header.action")),
        classes = "govuk-table__header govuk-table__header--numeric",
        attributes = Map("id" -> "action-header")
      )
    )

  def selectedDuties(selectedTaxCodes: Seq[TaxCode], changeLink: Call)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.selected-duties.question")),
          classes = "govuk-!-font-weight-bold govuk-!-padding-bottom-3"
        ),
        value = Value(
          content = HtmlContent(
            selectedTaxCodes
              .map(taxCode => s"${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}")
              .mkString("<br/>")
          ),
          classes = "govuk-!-margin-bottom-3"
        ),
        classes = "govuk-summary-list govuk-!-margin-bottom-3 govuk-summary-list--no-border",
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = changeLink.url,
                content = Text(messages("check-claim.change")),
                visuallyHiddenText = Some(messages("check-claim.selected-duties.question"))
              )
            )
          )
        )
      )
    )

  def claimsRowsForSingle(claims: Seq[ReimbursementWithCorrectAmount], claimAction: TaxCode => Call)(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, _, _) =>
      Seq(
        TableRow(
          content = HtmlContent(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}"),
          attributes = Map(
            "id" -> s"selected-claim-${taxCode.value}"
          ),
          classes = "govuk-table__cell"
        ),
        TableRow(
          content = Text(paidAmount.toPoundSterlingString),
          attributes = Map(
            "id" -> s"full-amount-${taxCode.value}"
          ),
          classes = "govuk-table__cell--numeric"
        ),
        TableRow(
          content = Text(claimAmount.toPoundSterlingString),
          attributes = Map(
            "id" -> s"claim-amount-${taxCode.value}"
          ),
          classes = "govuk-table__cell--numeric"
        ),
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
    claims.map {
      case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, _, Some(dutyType)) =>
        val suffix           = s"$dutyType-$taxCode"
        val hiddenChangeText =
          if (taxCode.exciseCategory.isEmpty)
            messages("check-claim.duty-claim-amount.hidden", s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}")
          else
            messages(
              "check-claim.duty-claim-amount.hidden",
              s"${messages(s"excise-category.${taxCode.exciseCategory.get.repr}")} ($taxCode)"
            )
        makeScheduledRowCells(taxCode, claimAmount, paidAmount, suffix, false) ++ Seq(
          TableRow(
            content = HtmlContent(
              messages(
                "check-claim.table.scheduled.change-link",
                claimAction(dutyType, taxCode).url,
                s"change-link-$suffix",
                hiddenChangeText
              )
            ),
            attributes = Map("id" -> s"change-$suffix"),
            classes = "govuk-link"
          )
        )
      case ReimbursementWithCorrectAmount(taxCode, claimAmount, paidAmount, _, None)           =>
        val suffix = s"$taxCode"
        makeScheduledRowCells(taxCode, claimAmount, paidAmount, suffix, false)
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
        attributes = Map("id" -> s"full-amount-total-$idSuffix"),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmountTotal.toPoundSterlingString),
        attributes = Map(
          "id" -> s"claim-amount-total-$idSuffix"
        ),
        classes = "govuk-table__cell govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(messages("check-claim.table-header.action")),
        attributes = Map("id" -> s"action-cell-$idSuffix"),
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
        attributes = Map(
          "id" -> s"selected-claim-$idSuffix"
        ),
        classes = "govuk-table__cell--regular"
      ),
      TableRow(
        content = Text(paidAmount.toPoundSterlingString),
        attributes = Map("id" -> s"full-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmount.toPoundSterlingString),
        attributes = Map("id" -> s"claim-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      )
    )

  private def makeScheduledRowCells(
    taxCode: TaxCode,
    claimAmount: BigDecimal,
    paidAmount: BigDecimal,
    idSuffix: String,
    headerBold: Boolean = true
  )(implicit messages: Messages): Seq[TableRow] =
    Seq(
      TableRow(
        content =
          if (taxCode.exciseCategory.isEmpty) HtmlContent(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}")
          else HtmlContent(s"${messages(s"excise-category.${taxCode.exciseCategory.get.repr}")} ($taxCode)"),
        attributes = Map(
          "id" -> s"selected-claim-$idSuffix"
        ),
        classes = if (headerBold) "govuk-table__header" else "govuk-!-font-weight-regular govuk-table__header"
      ),
      TableRow(
        content = Text(paidAmount.toPoundSterlingString),
        attributes = Map("id" -> s"full-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmount.toPoundSterlingString),
        attributes = Map("id" -> s"claim-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      )
    )

  private def makeCommonRowCellsSingleSecurity(
    taxCode: TaxCode,
    claimAmount: BigDecimal,
    paidAmount: BigDecimal,
    idSuffix: String
  )(implicit messages: Messages): Seq[TableRow] =
    Seq(
      TableRow(
        content = HtmlContent(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}"),
        attributes = Map(
          "id" -> s"selected-claim-$idSuffix"
        ),
        classes = "govuk-table__header govuk-!-font-weight-regular"
      ),
      TableRow(
        content = Text(paidAmount.toPoundSterlingString),
        attributes = Map("id" -> s"full-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      ),
      TableRow(
        content = Text(claimAmount.toPoundSterlingString),
        attributes = Map("id" -> s"claim-amount-$idSuffix"),
        classes = "govuk-table__cell--numeric"
      )
    )

  def claimsDutyTypesSummary(dutyTypes: Seq[DutyType], dutyTypesChangeCall: Call)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.duty-types-summary.key"))
        ),
        value = Value(
          HtmlContent(
            dutyTypes
              .map(dutyType =>
                Paragraph(
                  messages(s"select-duty-types.${dutyType.repr}")
                )
              )
              .mkString("")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = dutyTypesChangeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages("check-claim.duty-types-summary.hidden")
                )
              )
            )
          )
        )
      )
    )

  def claimsDutiesSelectedSummary(
    dutyType: String,
    claims: List[ReimbursementWithCorrectAmount],
    dutiesSelectedChangeCall: Call
  )(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(
            messages(s"select-duty-codes.title.$dutyType")
          )
        ),
        value = Value(
          HtmlContent(
            claims
              .map(claim =>
                Paragraph(
                  messages(s"tax-code.${claim.taxCode}")
                )
              )
              .mkString("")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = dutiesSelectedChangeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages("check-claim.duties-selected-summary.hidden", messages(s"select-duty-types.$dutyType"))
                )
              )
            )
          )
        )
      )
    )

  def claimsExciseCategoriesSummary(exciseCategories: List[String], exciseCategoriesChangeCall: Call)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(
            messages("select-duty-codes.title.excise-duty")
          )
        ),
        value = Value(
          HtmlContent(
            exciseCategories
              .map(exciseCategory =>
                Paragraph(
                  messages(s"excise-category.$exciseCategory")
                )
              )
              .mkString("")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = exciseCategoriesChangeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages("check-claim.duties-selected-summary.hidden", messages(s"select-duty-types.excise-duty"))
                )
              )
            )
          )
        )
      )
    )

  def claimsExciseDutiesSelectedSummary(
    exciseCategory: String,
    claims: List[ReimbursementWithCorrectAmount],
    dutiesSelectedChangeCall: Call
  )(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(
            messages(
              s"check-claim.duties-selected-summary.key",
              messages(s"select-excise-duty-codes.h1.$exciseCategory")
            )
          )
        ),
        value = Value(
          HtmlContent(
            claims
              .map(claim =>
                Paragraph(
                  messages(s"tax-code.${claim.taxCode}")
                )
              )
              .mkString("")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = dutiesSelectedChangeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages(
                    "check-claim.excise.duties-selected-summary.hidden",
                    messages(s"excise-category.$exciseCategory")
                  )
                )
              )
            )
          )
        )
      )
    )

  def claimsTotalSummary(claims: Seq[ReimbursementWithCorrectAmount])(implicit
    messages: Messages
  ): Seq[SummaryListRow] = claimsTotalSummary(claims.map(_.amount).sum)

  def claimsTotalSummarySingleSecurity(claims: Seq[ReimbursementWithCorrectAmount])(implicit
    messages: Messages
  ): Seq[SummaryListRow] = claimsTotalSummarySingleSecurity(claims.map(_.amount).sum)

  def claimsTotalSummaryScheduled(claims: Seq[ReimbursementWithCorrectAmount])(implicit
    messages: Messages
  ): Seq[SummaryListRow] = claimsTotalSummaryScheduled(claims.map(_.amount).sum)

  def claimsTotalSummary(total: BigDecimal)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.table.total")),
          classes = "govuk-!-padding-bottom-3 govuk-summary-list__key--nowrap"
        ),
        value = Value(
          content = Text(total.toPoundSterlingString),
          classes = "govuk-!-margin-bottom-3"
        ),
        classes = "govuk-summary-list govuk-!-margin-bottom-3 govuk-summary-list--no-border"
      )
    )

  def claimsTotalSummarySingleSecurity(total: BigDecimal)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.table.total"))
        ),
        value = Value(
          content = Text(total.toPoundSterlingString)
        ),
        classes = "govuk-summary-list govuk-summary-list--no-border"
      )
    )

  def claimsTotalSummaryScheduled(total: BigDecimal)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(
          content = HtmlContent(messages("check-claim.table.total"))
        ),
        value = Value(
          content = Text(total.toPoundSterlingString)
        ),
        classes = "govuk-summary-list govuk-summary-list--no-border"
      )
    )

  def makeSecurityClaimFullAmount(
    securityDepositId: String,
    reclaims: List[ReclaimWithAmounts],
    declaration: ImportDeclaration,
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

  def makeSingleSecurityClaimFullAmountAndSelectedDuties(
    securityDepositId: String,
    reclaims: List[ReclaimWithAmounts],
    availableDuties: Seq[DutyAmount],
    declaration: ImportDeclaration,
    key: String,
    fullAmountChangeCallOpt: Option[Call],
    selectDutiesChangeCallOpt: Option[Call]
  )(implicit messages: Messages): SummaryList = SummaryList(
    attributes = Map("id" -> s"claim-full-amount-selected-duties"),
    classes = "govuk-!-margin-bottom-9",
    rows = Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages("check-claim.securities.single.claiming-full-amount"))),
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
                attributes = Map("id" -> s"change-claim-full-amount"),
                href = fullAmountChangeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages("check-claim.securities.single.hidden.claim-full-amount")
                )
              )
            )
          )
        )
      )
    )
      ++ (if availableDuties.size == 1 then List.empty
          else
            Seq(
              SummaryListRow(
                key = Key(HtmlContent(messages("check-claim.securities.single.what-do-you-want-to-claim"))),
                value = Value(
                  HtmlContent(
                    reclaims
                      .map(reclaim =>
                        Paragraph(
                          messages(s"tax-code.${reclaim.taxCode.value}")
                        )
                      )
                      .mkString("")
                  )
                ),
                actions = selectDutiesChangeCallOpt.map(selectDutiesChangeCall =>
                  Actions(
                    items = Seq(
                      ActionItem(
                        attributes = Map("id" -> s"change-selected-duties"),
                        href = selectDutiesChangeCall.url,
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(
                          messages("check-claim.securities.single.hidden.select-duties")
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

  def claimsRowsForSingleSecurity(
    securityDepositId: String,
    claims: Seq[ReclaimWithAmounts],
    claimAction: (String, TaxCode) => Call
  )(implicit
    messages: Messages
  ): Seq[Seq[TableRow]] =
    claims.map { case ReclaimWithAmounts(taxCode, claimAmount, paidAmount) =>
      makeCommonRowCellsSingleSecurity(taxCode, claimAmount, paidAmount, taxCode.value) ++ Seq(
        TableRow(
          content = HtmlContent(
            messages(
              "check-claim.table.change-link",
              claimAction(securityDepositId, taxCode).url,
              s"change-link-${taxCode.value}"
            ) + visuallyHiddenText(
              messages(
                "check-claim.securities.single.hidden.duty-amount",
                messages(s"select-duties.duty.${taxCode.value}")
              )
            )
          ),
          attributes = Map("id" -> s"change-${taxCode.value}"),
          classes = "govuk-link govuk-table__cell--numeric"
        )
      )
    }

  def visuallyHiddenText(text: String): String =
    s"<span class='govuk-visually-hidden hmrc-a11y-hiddencopy'>$text</span>"

  def sortReimbursementsByDisplayDuty(
    reimbursements: SortedMap[DutyType, List[ReimbursementWithCorrectAmount]]
  ): Map[DutyType, List[ReimbursementWithCorrectAmount]] = {
    val ukDuties     = reimbursements.filter(_._1 === UkDuty).values.flatten.toList
    val euDuties     = reimbursements.filter(_._1 === EuDuty).values.flatten.toList
    val exciseDuties = reimbursements.filterNot(i => i._1 === UkDuty || i._1 === EuDuty).values.flatten.toList

    Map(
      UkDuty -> ukDuties,
      EuDuty -> euDuties,
      Excise -> exciseDuties
    ).filterNot(_._2.isEmpty)
  }

  // s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}"

  def chargesToClaimForMultiple(
    claims: Seq[ReimbursementWithCorrectAmount],
    mrn: MRN,
    index: Int,
    selectDutiesAction: Int => Call
  )(implicit messages: Messages): SummaryList =
    SummaryList(
      attributes = Map("id" -> s"charges-to-claim-$index"),
      classes = "govuk-summary-list--no-border",
      rows = Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-claim.multiple.what-do-you-want-to-claim"))),
          value = Value(
            HtmlContent(
              claims
                .map(r => s"<li>${r.taxCode} - ${messages(s"select-duties.duty.${r.taxCode}")}</li>")
                .mkString("<ul class=\"govuk-list\">", "", "</ul>")
            )
          ),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  attributes = Map("id" -> s"charges-to-claim-$index-action"),
                  href = selectDutiesAction(index).url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(
                    messages("check-claim.multiple.what-do-you-want-to-claim")
                  )
                )
              )
            )
          )
        )
      )
    )

}
