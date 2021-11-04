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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BigDecimalOps, ClaimedReimbursement}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{Actions, _}

import javax.inject.{Inject, Singleton}

@Singleton
class ClaimSummaryHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-claim-summary"

  def makeUkClaimSummary(answer: ClaimedReimbursementsAnswer): List[SummaryListRow] =
    makeClaimSummaryRows(answer.ukClaims(answer)) ++ makeTotalRow(answer.ukClaims(answer))

  def makeEuClaimSummary(answer: ClaimedReimbursementsAnswer): List[SummaryListRow] =
    makeClaimSummaryRows(answer.euClaims(answer)) ++ makeTotalRow(answer.euClaims(answer))

  def makeExciseClaimSummary(answer: ClaimedReimbursementsAnswer): List[SummaryListRow] =
    makeClaimSummaryRows(answer.exciseClaims(answer)) ++ makeTotalRow(answer.exciseClaims(answer))

  def makeClaimSummaryRows(claims: List[ClaimedReimbursement]): List[SummaryListRow] =
    claims.map { claim =>
      SummaryListRow(
        key = Key(Text(messages(s"select-duties.duty.${claim.taxCode}")(lang))),
        value = Value(Text(claim.claimAmount.toPoundSterlingString)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.EnterSingleClaimController.enterClaim(claim.id).url}",
                content = Text(messages("cya.change")(lang)),
                visuallyHiddenText = Some(messages(s"select-duties.duty.${claim.taxCode}")(lang))
              )
            )
          )
        )
      )
    }

  def makeTotalRow(claims: List[ClaimedReimbursement]): List[SummaryListRow] =
    SummaryListRow(
      key = Key(Text(messages(s"$key.total")(lang))),
      value = Value(Text(claims.map(_.claimAmount).sum.toPoundSterlingString))
    ) :: Nil

  def makeClaimTotalRow(claims: ClaimedReimbursementsAnswer): SummaryListRow =
    SummaryListRow(
      key = Key(Text(messages(s"$key.total")(lang))),
      value = Value(Text(claims.total.toPoundSterlingString))
    )

}
