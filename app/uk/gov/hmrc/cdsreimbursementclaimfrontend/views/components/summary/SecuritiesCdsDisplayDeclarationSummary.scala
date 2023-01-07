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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow

object SecuritiesCdsDisplayDeclarationSummary {

  def apply(
    declaration: DisplayDeclaration,
    key: String,
    mrnChangeCall: Call,
    rfsChangeCall: Call
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.mrn-label"))),
        value = Value(Text(declaration.displayResponseDetail.declarationId)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = mrnChangeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.mrn-label"))
              )
            )
          )
        )
      ).some,
      declaration.consigneeName.map { name =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.importer-name-label"))),
          value = Value(Text(name))
        )
      },
      declaration.consigneeEmail.map { email =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.importer-email-label"))),
          value = Value(Text(email))
        )
      },
      declaration.consigneeTelephone.map { telephone =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.importer-telephone-label"))),
          value = Value(Text(telephone))
        )
      },
      declaration.consigneeAddress.map { address =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.importer-address-label"))),
          value = Value(HtmlContent(address))
        )
      },
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.declarant-name-label"))),
        value = Value(Text(declaration.declarantName))
      ).some,
      declaration.declarantContactAddress.map { address =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.declarant-address-label"))),
          value = Value(HtmlContent(address))
        )
      },
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
