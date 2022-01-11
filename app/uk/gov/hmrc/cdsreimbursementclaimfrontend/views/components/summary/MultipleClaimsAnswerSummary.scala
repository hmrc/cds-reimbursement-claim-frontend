/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import play.api.mvc.Call

object MultipleClaimsAnswerSummary extends AnswerSummary[List[(MRN, ClaimedReimbursementsAnswer)]] {

  override def render(
    mrnsWithClaimsList: List[(MRN, ClaimedReimbursementsAnswer)],
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit messages: Messages): SummaryList = {

    val totalAmount: BigDecimal =
      mrnsWithClaimsList.flatMap(_._2.toList.map(_.claimAmount)).sum

    SummaryList(rows =
      mrnsWithClaimsList
        .map { case (mrn, claims) =>
          SummaryListRow(
            key = Key(Text(mrn.value)),
            value = Value(Text(claims.toList.map(_.claimAmount).sum.toPoundSterlingString)),
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
            )
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
}
