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

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

object SecuritiesCdsDisplayDeclarationClaimDetailsSummary {

  def apply(
    declaration: DisplayDeclaration,
    key: String,
    rfsChangeCall: Call
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      declaration.getReasonForSecurity.map(rfs =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.reason-for-security-label"))),
          value = Value(Text(messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}"))),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = rfsChangeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.reason-for-security-label"))
                )
              )
            )
          )
        )
      ),
      DateUtils
        .displayFormat(declaration.displayResponseDetail.acceptanceDate)
        .map(formattedDate =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.acceptance-date-label"))),
            value = Value(Text(formattedDate))
          )
        ),
      DateUtils
        .displayFormat(declaration.displayResponseDetail.btaDueDate)
        .map(formattedDate =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.bta-due-date-label"))),
            value = Value(Text(formattedDate))
          )
        )
    ).flatMap(_.toList)
  )
}
