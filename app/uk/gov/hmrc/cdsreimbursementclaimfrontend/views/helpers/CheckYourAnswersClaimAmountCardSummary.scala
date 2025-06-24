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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

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
          .map { summary =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"tax-code.${summary.taxCode}"))),
              value = Value(Text(summary.amount.toPoundSterlingString)),
              actions = claimAmountChangeCallOpt.map(changeCall =>
                Actions(
                  items = Seq(
                    ActionItem(
                      href = changeCall(summary.taxCode).url,
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(
                        messages(s"check-claim.duty-claim-amount.hidden", messages(s"tax-code.${summary.taxCode}"))
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
                s"<span class='govuk-body govuk-!-margin-bottom-0 govuk-!-font-weight-regular mrn-value'>${mrn.value}</span>"
            )
          ),
          value = Value(Text(claims.values.sum.toPoundSterlingString)),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some("")
                )
              )
            )
          )
        )
      }.toSeq ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.claim-total.total"))),
            value = Value(Text(reimbursementsMap.flatMap(_._2.values).sum.toPoundSterlingString))
          )
        )
    )
}
