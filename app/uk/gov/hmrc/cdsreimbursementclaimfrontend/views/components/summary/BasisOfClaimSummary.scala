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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class BasisOfClaimSummary extends AnswerSummary[BasisOfClaim] {

  def render(key: String, answer: BasisOfClaim)(implicit journey: JourneyBindable, messages: Messages): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.l0"))),
          value = Value(Text(messages(s"select-basis-for-claim.reason.d${answer.value}"))),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = routes.SelectBasisForClaimController.changeBasisForClaim(journey).url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.l0"))
                )
              )
            )
          )
        )
      )
    )
}
