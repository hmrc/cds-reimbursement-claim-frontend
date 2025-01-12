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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow

object SecurityDetailsSummary {

  def apply(
    declaration: DisplayDeclaration,
    securityDepositId: String,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList = {

    val securityDetailsOpt = declaration.getSecurityDetailsFor(securityDepositId)

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.mrn-label"))),
          value = Value(Text(declaration.displayResponseDetail.declarationId))
        ).some,
        declaration.displayResponseDetail.declarantReferenceNumber.map(lrn =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.lrn-label"))),
            value = Value(Text(lrn))
          )
        ),
        declaration.getReasonForSecurity.map(rfs =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.reason-for-security-label"))),
            value = Value(Text(messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")))
          )
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.total-amount-label"))),
          value = Value(Text(declaration.getSecurityTotalValueFor(securityDepositId).toPoundSterlingString))
        ).some,
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.paid-amount-label"))),
          value = Value(Text(declaration.getSecurityPaidValueFor(securityDepositId).toPoundSterlingString))
        ).some,
        DateUtils
          .displayFormat(declaration.displayResponseDetail.acceptanceDate)
          .map(formattedDate =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.acceptance-date-label"))),
              value = Value(Text(toDisplayDate(formattedDate)))
            )
          ),
        DateUtils
          .displayFormat(declaration.displayResponseDetail.btaDueDate)
          .map(formattedDate =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.bta-due-date-label"))),
              value = Value(Text(toDisplayDate(formattedDate)))
            )
          )
      ).flatMap(_.toList)
    )
  }
}
