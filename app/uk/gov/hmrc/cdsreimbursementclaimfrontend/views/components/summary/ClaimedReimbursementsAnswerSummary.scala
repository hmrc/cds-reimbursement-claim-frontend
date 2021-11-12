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

import cats.implicits.catsSyntaxEq
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class ClaimedReimbursementsAnswerSummary extends AnswerSummary[ClaimedReimbursementsAnswer] {

  def render(key: String, reimbursements: ClaimedReimbursementsAnswer)(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList = {

    val amendCall =
      if (router.journeyBindable === JourneyBindable.Scheduled)
        claimsRoutes.CheckScheduledClaimController.showReimbursements()
      else claimsRoutes.EnterSingleClaimController.checkClaimSummary()

    val individualClaimSummaries = DutyTypeSummary.buildFrom(reimbursements)

    SummaryList(rows =
      individualClaimSummaries
        .map { summary =>
          SummaryListRow(
            key = Key(Text(messages(s"$key.${summary.messageKey}"))),
            value = Value(Text(summary.total.toPoundSterlingString)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = amendCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.${summary.messageKey}"))
                  )
                )
              )
            )
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(Text(messages(s"$key.total"))),
            value = Value(Text(individualClaimSummaries.map(_.total).sum.toPoundSterlingString)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = amendCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.total"))
                  )
                )
              )
            )
          )
        )
    )
  }
}
