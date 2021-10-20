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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.routes
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class ReimbursementMethodAnswerSummary extends AnswerSummary[ReimbursementMethodAnswer] {
  def render(key: String, answer: ReimbursementMethodAnswer)(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList = {
    val label = messages(s"$key.label")

    SummaryList(
      List(
        SummaryListRow(
          key = Key(Text(label)),
          value = Value(Text(messages(answerKey(key, answer)))),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.ReimbursementMethodController.showReimbursementMethod()}",
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

  private def answerKey(key: String, answer: ReimbursementMethodAnswer): String = answer match {
    case CurrentMonthAdjustment => s"$key.cma"
    case BankAccountTransfer    => s"$key.bt"
  }
}
