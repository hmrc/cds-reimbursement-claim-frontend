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
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

object MovementReferenceNumberSummary {

  def single(answer: MRN, key: String, subKey: Option[String], changeCallOpt: Option[Call])(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.single.mrn-label"))),
          value = Value(Text(answer.value)),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages("check-your-answers.single.mrn-label-plaintext"))
                )
              )
            )
          )
        )
      )
    )

  def multiple(mrns: Seq[MRN], key: String, subKey: Option[String], changeCallOpt: Option[Call])(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      mrns.zipWithIndex.map { case (mrn, index) =>
        SummaryListRow(
          key = Key(
            HtmlContent(OrdinalNumberMrnHelper(index + 1))
          ),
          value = Value(Text(mrn.value)),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText =
                    Some(messages("check-your-answers.multiple.mrn-label-plaintext", OrdinalNumberMrnHelper(index + 1)))
                )
              )
            )
          )
        )
      }
    )

  def scheduled(answer: MRN, key: String, subKey: Option[String], changeCallOpt: Option[Call])(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.scheduled.mrn-label"))),
          value = Value(Text(answer.value)),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages("check-your-answers.scheduled.mrn-label-plaintext"))
                )
              )
            )
          )
        )
      )
    )
}
