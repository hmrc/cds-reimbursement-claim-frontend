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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object PayeeTypeSummary {

  def apply(
    answer: Option[PayeeType],
    key: String,
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    answer match {
      case Some(a) => apply(a, key, changeCallOpt)
      case None    => SummaryList(Nil)
    }

  def apply(
    answer: PayeeType,
    key: String,
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {
    val label = messages(s"$key.label")

    SummaryList(
      List(
        SummaryListRow(
          key = Key(Text(label)),
          value = Value(HtmlContent(messages(answerKey(answer)))),
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

  def answerKey(answer: PayeeType): String = answer match {
    case PayeeType.Consignee      => "choose-payee-type.radio.importer"
    case PayeeType.Declarant      => "choose-payee-type.radio.declarant"
    case PayeeType.Representative => "choose-payee-type.radio.representative"
  }
}
