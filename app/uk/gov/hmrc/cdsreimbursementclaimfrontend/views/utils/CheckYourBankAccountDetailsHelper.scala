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

import cats.implicits._
import play.api.i18n.{Lang, Langs, MessagesApi}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountAcc14

import javax.inject.{Inject, Singleton}

@Singleton
class CheckYourBankAccountDetailsHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "bank-details"

  def bankAccountDetailsSummary(
    displayBankAccountDetails: BankAccountAcc14,
    router: ReimbursementRoutes
  ): List[SummaryListRow] =
    List(
      makeAccountHolderNameRow(displayBankAccountDetails.accountName, router),
      makeSortCodeRow(displayBankAccountDetails.sortCode),
      makeAccountNumberRow(displayBankAccountDetails.accountNumber)
    ).flattenOption

  private def makeAccountHolderNameRow(accountHolderName: String, router: ReimbursementRoutes): Option[SummaryListRow] =
    Some(
      SummaryListRow(
        Key(Text(messages(s"$key.account-name.label")(lang))),
        Value(Text(accountHolderName)),
        "",
        Some(
          Actions(
            "govuk-link",
            List(
              ActionItem(
                //TODO: Change routing for this link to go to the 'Enter account type' page
                href = s"${routes.SelectBankAccountTypeController.selectBankAccountType(router.journeyBindable).url}",
                Text(messages("bank-details.change")(lang))
              )
            )
          )
        )
      )
    )

  private def makeSortCodeRow(sortCode: String): Option[SummaryListRow] =
    Some(
      SummaryListRow(
        Key(Text(messages(s"$key.sort-code.label")(lang))),
        Value(Text(sortCode))
      )
    )

  private def makeAccountNumberRow(accountNumber: String): Option[SummaryListRow] =
    Some(
      SummaryListRow(
        Key(Text(messages(s"$key.account-number.label")(lang))),
        Value(Text(accountNumber))
      )
    )

}
