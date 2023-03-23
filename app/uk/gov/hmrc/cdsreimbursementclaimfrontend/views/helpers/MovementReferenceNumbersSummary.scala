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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMrn
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.LeadMrn
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.MessagesHelper.combine
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
object MovementReferenceNumbersSummary extends AnswerSummary[List[MRN]] {

  override def render(mrns: List[MRN], key: String, subKey: Option[String], changeCallOpt: Option[Call])(implicit
    messages: Messages
  ): SummaryList = {

    def toLeadMrnSummary: LeadMrn => SummaryListRow = leadMrn =>
      SummaryListRow(
        key = Key(HtmlContent(messages(combine(key, subKey, "label")))),
        value = Value(Text(leadMrn.value)),
        actions = changeCallOpt.map(changeCall =>
          Actions(items =
            Seq(
              ActionItem(
                href = changeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages("check-your-answers.reference-number.multiple.label-plaintext"))
              )
            )
          )
        )
      )

    def toAssociatedMrnSummary: (AssociatedMrn, AssociatedMrnIndex) => SummaryListRow = (mrn, mrnIndex) => {
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.associated-mrn-label", mrnIndex.ordinalNumeral.capitalize))),
        value = Value(Text(mrn.value)),
        actions = Some(
          Actions(items =
            Seq(
              ActionItem(
                href = s"${overpaymentsMultipleRoutes.EnterAssociatedMrnController.changeMrn(mrnIndex).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages(
                    "check-your-answers.reference-number.associated-mrn-label-plaintext",
                    mrnIndex.ordinalNumeral.capitalize
                  )
                )
              )
            )
          )
        )
      )
    }

    SummaryList(
      mrns.headOption.map(toLeadMrnSummary).toList ++
        mrns.drop(1).zipWithIndex.map { case (mrn, index) =>
          toAssociatedMrnSummary(mrn, AssociatedMrnIndex.fromListIndex(index))
        }
    )
  }
}
