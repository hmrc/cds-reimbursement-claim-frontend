/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{AssociatedMrn, LeadMrn}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Index, IntegerOps}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.LanguageHelper.lang
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class MovementReferenceNumbersSummary extends AnswerSummary[Seq[MRN]] {

  def render(key: String, answers: Seq[MRN])(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList = {

    def toLeadMrnSummary: LeadMrn => SummaryListRow = leadMrn =>
      SummaryListRow(
        key = Key(Text(messages(lang(key, router.subKey, "label")))),
        value = Value(Text(leadMrn.value)),
        actions = Some(
          Actions(items =
            Seq(
              ActionItem(
                href = s"${routes.EnterMovementReferenceNumberController.enterJourneyMrn(router.journeyBindable).url}",
                content = Text(messages("cya.change"))
              )
            )
          )
        )
      )

    def toAssociatedMrnSummary: (AssociatedMrn, Index) => SummaryListRow = (mrn, index) => {
      val mrnIndex = index + 2
      SummaryListRow(
        key = Key(Text(messages(s"$key.associated-mrn-label", mrnIndex.ordinalNaming.capitalize))),
        value = Value(Text(mrn.value)),
        actions = Some(
          Actions(items =
            Seq(
              ActionItem(
                href = s"${routes.EnterAssociatedMrnController.enterMRN(mrnIndex).url}",
                content = Text(messages("cya.change"))
              )
            )
          )
        )
      )
    }

    SummaryList(
      answers.headOption.map(toLeadMrnSummary).toList ++
        answers.drop(1).zipWithIndex.map(toAssociatedMrnSummary.tupled)
    )
  }
}
