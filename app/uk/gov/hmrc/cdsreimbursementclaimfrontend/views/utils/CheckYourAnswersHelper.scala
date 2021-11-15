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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import cats.syntax.all._
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.{Inject, Singleton}

@Singleton
class CheckYourAnswersHelper @Inject() (implicit val featureSwitch: FeatureSwitchService) {

  private val key = "check-your-answers"

  def makeBankDetailsSummary(
    claim: DraftClaim
  )(implicit messages: Messages, journey: JourneyBindable): List[SummaryListRow] =
    List(
      claim.bankAccountDetailsAnswer.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-name.label"))),
          value = Value(Text(details.accountName.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails(journey).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-name.label"))
                )
              )
            )
          )
        )
      },
      claim.bankAccountDetailsAnswer.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.sort-code.label"))),
          value = Value(Text(details.sortCode.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails(journey).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.sort-code.label"))
                )
              )
            )
          )
        )
      },
      claim.bankAccountDetailsAnswer.map { details =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.bank-details.account-number.label"))),
          value = Value(Text(details.accountNumber.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.BankAccountController.checkBankAccountDetails(journey).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.bank-details.account-number.label"))
                )
              )
            )
          )
        )
      }
    ).flattenOption
}
