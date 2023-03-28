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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

object ReimbursementClaimsRejectedGoodsSummary {
  def singleFull(
    reimbursementClaims: Seq[(TaxCode, BigDecimal)],
    key: String,
    enterClaimAction: TaxCode => Call
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(rows =
      reimbursementClaims
        .map { case (taxCode, amount) =>
          SummaryListRow(
            key = Key(Text(messages(s"tax-code.$taxCode"))),
            value = Value(Text(amount.toPoundSterlingString)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = enterClaimAction(taxCode).url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"tax-code.$taxCode"))
                  )
                )
              )
            )
          )
        } ++ Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.single.total"))),
          value = Value(Text(reimbursementClaims.map(_._2).sum.toPoundSterlingString))
        )
      )
    )

  def singleForCYA(
    reimbursementClaims: Map[TaxCode, BigDecimal],
    key: String,
    changeCallOpt: Option[Call]
  )(implicit messages: Messages): SummaryList =
    SummaryList(rows =
      reimbursementClaims.toSeq
        .map { case (taxCode, amount) =>
          SummaryListRow(
            key = Key(Text(messages(s"tax-code.$taxCode"))),
            value = Value(Text(amount.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"tax-code.$taxCode"))
                  )
                )
              )
            )
          )
        } ++ Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.total"))),
          value = Value(Text(reimbursementClaims.values.sum.toPoundSterlingString)),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.total"))
                )
              )
            )
          )
        )
      )
    )

  def multipleOverallTotalRow(
    reimbursementClaims: Seq[(MRN, Int, Map[TaxCode, BigDecimal])],
    key: String
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(Text(messages(s"$key.multiple.overall-total.label"))),
        value = Value(
          HtmlContent(
            s"""<span id="overall-total">${reimbursementClaims.flatMap(_._3.values).sum.toPoundSterlingString}</span>"""
          )
        )
      )
    ),
    "govuk-!-margin-bottom-9"
  )

  def multipleForCYA(
    reimbursementClaims: Map[MRN, Map[TaxCode, BigDecimal]],
    key: String,
    changeCallOpt: Option[Call] = None
  )(implicit
    messages: Messages
  ): SummaryList = {
    val totalAmount: BigDecimal =
      reimbursementClaims.flatMap(_._2.values).sum

    SummaryList(rows =
      reimbursementClaims.toSeq
        .map { case (mrn, claims) =>
          SummaryListRow(
            key = Key(Text(mrn.value)),
            value = Value(Text(claims.values.sum.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.duty.label", mrn.value))
                  )
                )
              )
            ),
            classes = "summary-key-mrn"
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(Text(messages(s"$key.multiple.total"))),
            value = Value(Text(totalAmount.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.multiple.total"))
                  )
                )
              )
            )
          )
        )
    )
  }

  def scheduledForCYA(
    reimbursementClaims: Map[DutyType, Map[TaxCode, AmountPaidWithRefund]],
    key: String,
    changeCallOpt: Option[Call] = None
  )(implicit
    messages: Messages
  ): SummaryList = {

    val amountsPerDutyType: Seq[(DutyType, BigDecimal)] =
      reimbursementClaims.view.mapValues(_.values.map(_.refundAmount).sum).toSeq

    val totalAmount: BigDecimal =
      reimbursementClaims.flatMap(_._2.values.map(_.refundAmount)).sum

    SummaryList(rows =
      amountsPerDutyType
        .map { case (dutyType, amount) =>
          SummaryListRow(
            key = Key(Text(messages(s"duty-type.${dutyType.repr}"))),
            value = Value(Text(amount.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"duty-type.${dutyType.repr}"))
                  )
                )
              )
            ),
            classes = "summary-key-duty-type"
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(Text(messages(s"$key.scheduled.total"))),
            value = Value(Text(totalAmount.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.scheduled.total"))
                  )
                )
              )
            )
          )
        )
    )
  }
}
