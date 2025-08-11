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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import scala.collection.immutable.SortedMap

object CheckYourAnswersClaimAmountCardSummary {

  def renderForSingle(
    reimbursements: Seq[Reimbursement],
    dutiesSelectedChangeCallOpt: Option[Call],
    claimAmountChangeCallOpt: Option[TaxCode => Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.duties-selected"))),
          value = Value(
            HtmlContent(
              reimbursements
                .map(reimbursement =>
                  Paragraph(
                    messages(s"tax-code.${reimbursement.taxCode}")
                  )
                )
                .mkString("")
            )
          ),
          actions = dutiesSelectedChangeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"check-claim.duty-types-summary.hidden"))
                )
              )
            )
          )
        )
      ) ++
        reimbursements
          .map { reimbursement =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"tax-code.${reimbursement.taxCode}"))),
              value = Value(Text(reimbursement.amount.toPoundSterlingString)),
              actions = claimAmountChangeCallOpt.map(changeCall =>
                Actions(
                  items = Seq(
                    ActionItem(
                      href = changeCall(reimbursement.taxCode).url,
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(
                        messages(
                          s"check-claim.duty-claim-amount.hidden",
                          messages(s"tax-code.${reimbursement.taxCode}")
                        )
                      )
                    )
                  )
                )
              )
            )
          } ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.claim-total.total"))),
            value = Value(Text(reimbursements.map(_.amount).sum.toPoundSterlingString))
          )
        )
    )

  def renderForMultiple(
    reimbursementsMap: Map[MRN, Map[TaxCode, BigDecimal]],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      reimbursementsMap.zipWithIndex.map { case ((mrn, claims), index) =>
        SummaryListRow(
          key = Key(
            HtmlContent(
              s"${OrdinalNumberMrnHelper.apply(index + 1)}" + "<br>" +
                s"<span class='govuk-body govuk-!-margin-bottom-0 govuk-!-font-weight-regular'>${mrn.value}</span>"
            ),
            classes = "cya-multiple-claim-amount-mrn-key"
          ),
          value = Value(Text(claims.values.sum.toPoundSterlingString), classes = "responsive-text-align"),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText =
                    Some(messages("check-your-answers.claim-amount.amount.hidden", OrdinalNumberMrnHelper(index + 1)))
                )
              )
            )
          )
        )
      }.toSeq ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.claim-total.total"))),
            value = Value(
              Text(reimbursementsMap.flatMap(_._2.values).sum.toPoundSterlingString),
              classes = "responsive-text-align"
            )
          )
        )
    )

  def renderForScheduled(
    reimbursementClaims: Map[DutyType, Map[TaxCode, AmountPaidWithCorrect]],
    changeCallOpt: Option[Call] = None
  )(implicit
    messages: Messages
  ): SummaryList = {

    val amountsPerDutyCategory: Seq[(String, BigDecimal)] =
      reimbursementClaims.map { case (dutyType, reimbursements) =>
        val category = dutyType.repr
        val amount   = reimbursements.map(_._2.claimAmount).sum
        (category, amount)
      }.toSeq

    val totalAmount: BigDecimal =
      amountsPerDutyCategory.map(_._2).sum

    SummaryList(rows =
      amountsPerDutyCategory
        .map { case (category, amount) =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"check-your-answers.claim-amount.$category.label"))),
            value = Value(Text(amount.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(
                      messages(
                        "check-your-answers.claim-amount.amount.hidden",
                        messages(s"check-your-answers.claim-amount.$category.label")
                      )
                    )
                  )
                )
              )
            )
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.claim-total.total"))),
            value = Value(Text(totalAmount.toPoundSterlingString))
          )
        )
    )
  }

  def renderForSecurities(
    reimbursements: SortedMap[TaxCode, BigDecimal],
    dutiesSelectedChangeCallOpt: Option[Call],
    claimAmountChangeCallOpt: Option[TaxCode => Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.duties-selected"))),
          value = Value(
            HtmlContent(
              reimbursements
                .map((taxCode, _) =>
                  Paragraph(
                    messages(s"tax-code.$taxCode")
                  )
                )
                .mkString("")
            )
          ),
          actions = dutiesSelectedChangeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"check-claim.duty-types-summary.hidden"))
                )
              )
            )
          )
        )
      ) ++
        reimbursements
          .map { (taxCode, amount) =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"tax-code.$taxCode"))),
              value = Value(Text(amount.toPoundSterlingString)),
              actions = claimAmountChangeCallOpt.map(changeCall =>
                Actions(
                  items = Seq(
                    ActionItem(
                      href = changeCall(taxCode).url,
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(
                        messages(
                          s"check-claim.duty-claim-amount.hidden",
                          messages(s"tax-code.$taxCode")
                        )
                      )
                    )
                  )
                )
              )
            )
          } ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.claim-total.total"))),
            value = Value(Text(reimbursements.map(_._2).sum.toPoundSterlingString))
          )
        )
    )
}
