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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import cats.data.NonEmptyList
import play.api.i18n.Lang
import play.api.i18n.Langs
import play.api.i18n.MessagesApi
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class ClaimSummaryHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-claim-summary"

  def makeClaimSummary(claims: NonEmptyList[ClaimedReimbursement]): List[SummaryListRow] =
    makeClaimSummaryRows(claims) ++ makeTotalRow(claims)

  def makeClaimSummaryRows(claims: NonEmptyList[ClaimedReimbursement]): List[SummaryListRow] =
    claims.toList.map { claim =>
      SummaryListRow(
        key = Key(Text(s"${claim.taxCode} - ${messages(s"select-duties.duty.${claim.taxCode}")(lang)}")),
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

  def makeTotalRow(claims: NonEmptyList[ClaimedReimbursement]): List[SummaryListRow] =
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.total")(lang))),
      value = Value(Text(claims.toList.map(_.claimAmount).sum.toPoundSterlingString)),
      classes = "govuk-!-margin-bottom-9"
    ) :: Nil

}
