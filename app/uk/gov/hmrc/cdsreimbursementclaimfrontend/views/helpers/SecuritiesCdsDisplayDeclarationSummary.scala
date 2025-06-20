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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
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
    mrnChangeCallOpt: Option[Call],
    rfsChangeCallOpt: Option[Call],
    showImporterDeclarantDetails: Boolean = true
  )(implicit messages: Messages) =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.securities.mrn-label"))),
          value = Value(Text(declaration.displayResponseDetail.declarationId)),
          actions = mrnChangeCallOpt.map(mrnChangeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = mrnChangeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.mrn-label"))
                )
              )
            )
          ),
          classes = "mrn-value"
        ).some,
        declaration.getMaybeLRN match {
          case Some(lrn) =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.securities.lrn-label"))),
              value = Value(Text(lrn)),
              classes = "mrn-value"
            ).some
          case _         => None
        },
        declaration.consigneeName
          .map { name =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.importer-name-label"))),
              value = Value(Text(name))
            )
          }
          .filter(_ => showImporterDeclarantDetails),
        declaration.consigneeEmail
          .map { email =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.importer-email-label"))),
              value = Value(Text(email))
            )
          }
          .filter(_ => showImporterDeclarantDetails),
        declaration.consigneeTelephone
          .map { telephone =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.importer-telephone-label"))),
              value = Value(Text(telephone))
            )
          }
          .filter(_ => showImporterDeclarantDetails),
        declaration.consigneeAddress
          .map { address =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.importer-address-label"))),
              value = Value(HtmlContent(address))
            )
          }
          .filter(_ => showImporterDeclarantDetails),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.declarant-name-label"))),
          value = Value(Text(declaration.declarantName))
        ).some.filter(_ => showImporterDeclarantDetails),
        declaration.declarantContactAddress
          .map { address =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.declarant-address-label"))),
              value = Value(HtmlContent(address))
            )
          }
          .filter(_ => showImporterDeclarantDetails),
        declaration.getReasonForSecurity.map(rfs =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.reason-for-security-label"))),
            value = Value(Text(messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}"))),
            actions = rfsChangeCallOpt.map(rfsChangeCall =>
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
