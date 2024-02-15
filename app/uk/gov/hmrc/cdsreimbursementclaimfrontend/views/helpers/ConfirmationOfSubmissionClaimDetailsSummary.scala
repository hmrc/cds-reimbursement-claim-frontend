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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

object ConfirmationOfSubmissionClaimDetailsSummary {

  def apply(
    key: String,
    totalReimbursementAmount: BigDecimal,
    caseNumber: String,
    maybeMrn: Option[String] = None,
    subKey: Option[String] = None
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.reimbursement-amount"))),
        value = Value(Text(totalReimbursementAmount.toPoundSterlingString))
      ),
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-reference"))),
        value = Value(Text(caseNumber))
      ),
      SummaryListRow(
        key = if (subKey.getOrElse("") == "multiple") {
          Key(HtmlContent(messages(s"$key.multiple.mrn")))
        } else {
          Key(HtmlContent(messages(s"$key.mrn")))
        },
        value = Value(Text(maybeMrn.getOrElse("")))
      )
    )
  )
}
