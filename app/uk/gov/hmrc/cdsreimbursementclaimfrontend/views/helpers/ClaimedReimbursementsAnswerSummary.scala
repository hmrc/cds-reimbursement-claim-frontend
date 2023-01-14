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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import play.api.mvc.Call

object ClaimedReimbursementsAnswerSummary extends AnswerSummary[ClaimedReimbursementsAnswer] {

  override def render(
    reimbursements: ClaimedReimbursementsAnswer,
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {

    val individualClaimSummaries = DutyTypeSummary.buildFrom(reimbursements)

    SummaryList(rows =
      individualClaimSummaries
        .map { summary =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.${summary.messageKey}"))),
            value = Value(Text(summary.total.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.${summary.messageKey}"))
                  )
                )
              )
            )
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.total"))),
            value = Value(Text(individualClaimSummaries.map(_.total).sum.toPoundSterlingString)),
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
  }
}
