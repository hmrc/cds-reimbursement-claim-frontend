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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.LanguageHelper.lang
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumber

object MovementReferenceNumberSequenceSummary {

  def apply(mrns: Seq[MRN], key: String, subKey: Option[String], changeCallOpt: Option[Int => Call])(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      mrns.zipWithIndex.map { case (mrn, index) =>
        SummaryListRow(
          key = Key(Text(messages(lang(key, subKey, "label"), OrdinalNumber.label(index + 1).capitalize))),
          value = Value(Text(mrn.value)),
          actions = changeCallOpt.map(changeCallFx =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCallFx(index).url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(lang(key, subKey, "label"), OrdinalNumber.label(index)))
                )
              )
            )
          )
        )
      }
    )
}
