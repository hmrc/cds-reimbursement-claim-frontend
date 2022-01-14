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
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

object ReimbursementsClaimsSummary extends AnswerSummary[Seq[(TaxCode, BigDecimal)]] {

  override def render(
    reimbursementClaims: Seq[(TaxCode, BigDecimal)],
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(rows =
      reimbursementClaims
        .map { case (taxCode, amount) =>
          SummaryListRow(
            key = Key(Text(messages(s"tax-code.${taxCode.value}"))),
            value = Value(Text(amount.toPoundSterlingString)),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"tax-code.${taxCode.value}"))
                  )
                )
              )
            )
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(Text(messages(s"$key.total"))),
            value = Value(Text(reimbursementClaims.map(_._2).sum.toPoundSterlingString)),
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