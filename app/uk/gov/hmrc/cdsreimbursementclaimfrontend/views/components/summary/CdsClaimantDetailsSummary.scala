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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class CdsClaimantDetailsSummary extends AnswerSummary[DetailsRegisteredWithCdsAnswer] {

  def render(key: String, answer: DetailsRegisteredWithCdsAnswer)(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l0"))),
          value = Value(Text(answer.fullName)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCds().url}",
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l0"))
                )
              )
            )
          )
        ),
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l1"))),
          value = Value(Text(answer.emailAddress.value)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCds().url}",
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l1"))
                )
              )
            )
          )
        ),
        SummaryListRow(
          key = Key(Text(messages(s"$key.claimant-details.l3"))),
          value = Value(
            Text(
              answer.contactAddress
                .getAddressLines(messages)
                .mkString(", ")
            )
          ),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCds().url}",
                  visuallyHiddenText = Some(messages(s"$key.claimant-details.l3"))
                )
              )
            )
          )
        )
      )
    )

}
