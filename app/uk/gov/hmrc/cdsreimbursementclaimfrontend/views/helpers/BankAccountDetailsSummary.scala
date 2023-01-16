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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import play.api.mvc.Call

object BankAccountDetailsSummary extends AnswerSummary[BankAccountDetails] {

  override def render(
    bankAccountDetails: BankAccountDetails,
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.account-name.label"))),
          value = Value(Text(bankAccountDetails.accountName.value)),
          classes = "govuk-summary-list__row--no-border",
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.account-name.label.hidden"))
                )
              )
            )
          )
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.sort-code.label"))),
          value = Value(Text(bankAccountDetails.sortCode.masked)),
          classes = "govuk-summary-list__row--no-border"
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.account-number.label"))),
          value = Value(Text(bankAccountDetails.accountNumber.masked))
        )
      )
    )
}
