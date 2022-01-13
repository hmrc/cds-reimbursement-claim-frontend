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
import play.api.mvc.Call
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

object SpecialCircumstancesSummary extends AnswerSummary[String] {

  override def render(answer: String, key: String, subKey: Option[String], changeCallOpt: Option[Call])(implicit
    messages: Messages
  ): SummaryList = {
    val label = messages(s"$key.label")

    SummaryList(
      List(
        SummaryListRow(
          key = Key(Text(label)),
          value = Value(Text(answer)),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(label)
                )
              )
            )
          )
        )
      )
    )
  }
}
