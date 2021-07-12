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

import play.api.i18n.{Lang, Langs, MessagesApi}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{Actions, _}

import javax.inject.{Inject, Singleton}

@Singleton
class ClaimSummaryHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-claim-summary"

  def makeUkClaimSummary(claimsAnswer: ClaimsAnswer): List[SummaryListRow] =
    makeTotalRow(claimsAnswer.ukClaims(claimsAnswer)) :: makeClaimSummaryRows(claimsAnswer.ukClaims(claimsAnswer))

  def makeEuClaimSummary(claimsAnswer: ClaimsAnswer): List[SummaryListRow] =
    makeTotalRow(claimsAnswer.euClaims(claimsAnswer)) :: makeClaimSummaryRows(claimsAnswer.euClaims(claimsAnswer))

  def makeExciseClaimSummary(claimsAnswer: ClaimsAnswer): List[SummaryListRow] =
    makeTotalRow(
      claimsAnswer.exciseClaims(claimsAnswer)
    ) :: makeClaimSummaryRows(claimsAnswer.exciseClaims(claimsAnswer))

  def makeClaimSummaryRows(claims: List[Claim]): List[SummaryListRow] =
    claims.map { claim =>
      SummaryListRow(
        key = Key(Text(messages(s"select-duties.duty.${claim.taxCode}")(lang))),
        value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(claim.claimAmount))),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterClaimController.enterClaim(claim.id).url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"select-duties.duty.${claim.taxCode}")(lang))
              )
            )
          )
        )
      )
    }

  def makeTotalRow(claims: List[Claim]): SummaryListRow =
    SummaryListRow(
      key = Key(Text(messages(s"$key.total")(lang))),
      value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(claims.map(_.claimAmount).sum)))
    )

  def makeClaimTotalRow(claims: ClaimsAnswer): SummaryListRow =
    SummaryListRow(
      key = Key(Text(messages(s"$key.total")(lang))),
      value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(claims.total)))
    )

}
