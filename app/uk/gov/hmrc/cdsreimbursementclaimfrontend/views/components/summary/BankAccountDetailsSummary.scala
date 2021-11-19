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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.checkYourAnswersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class BankAccountDetailsSummary extends AnswerSummary[BankAccountDetails] {

  def render(key: String, bankAccountDetails: BankAccountDetails)(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList = {

    def changeCall =
      if (key.contains(checkYourAnswersKey))
        routes.BankAccountController.checkBankAccountDetails(router.journeyBindable)
      else routes.SelectBankAccountTypeController.selectBankAccountType(router.journeyBindable)

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.account-name.label"))),
          value = Value(Text(bankAccountDetails.accountName.value)),
          classes = "govuk-summary-list__row--no-border",
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.account-name.label"))
                )
              )
            )
          )
        ),
        SummaryListRow(
          key = Key(Text(messages(s"$key.sort-code.label"))),
          value = Value(Text(bankAccountDetails.sortCode.value)),
          classes = "govuk-summary-list__row--no-border"
        ),
        SummaryListRow(
          key = Key(Text(messages(s"$key.account-number.label"))),
          value = Value(Text(bankAccountDetails.accountNumber.value))
        )
      )
    )
  }
}
