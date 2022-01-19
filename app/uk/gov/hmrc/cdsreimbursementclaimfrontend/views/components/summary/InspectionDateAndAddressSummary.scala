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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import play.api.mvc.Call
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

object InspectionDateAndAddressSummary {

  val ukDateFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("d MMMM yyyy").withLocale(Locale.UK)

  def apply(
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    key: String,
    changeInspectionDateCall: Option[Call] = None,
    changeInspectionAddressCall: Option[Call] = None,
    changeInspectionAddressTypeCall: Option[Call] = None
  )(implicit
    messages: Messages
  ): SummaryList = {

    val changeInspectionDateAction = changeInspectionDateCall.map(call =>
      Actions(
        items = Seq(
          ActionItem(
            href = s"${call.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.change-hint.inspection-date"))
          )
        )
      )
    )

    val changeInspectionAddressAction = changeInspectionAddressCall.map(call =>
      Actions(
        items = Seq(
          ActionItem(
            href = s"${call.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.change-hint.inspection-address"))
          )
        )
      )
    )

    val changeInspectionAddressTypeAction = changeInspectionAddressTypeCall.map(call =>
      Actions(
        items = Seq(
          ActionItem(
            href = s"${call.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.change-hint.inspection-address-type"))
          )
        )
      )
    )

    val addressData = List(
      Paragraph(inspectionAddress.addressLine1),
      Paragraph(inspectionAddress.addressLine2),
      Paragraph(inspectionAddress.city),
      Paragraph(inspectionAddress.postalCode)
    )

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.inspection-date"))),
          value = Value(Text(inspectionDate.value.toString)),
          actions = changeInspectionDateAction
        ),
        SummaryListRow(
          key = Key(Text(messages(s"$key.inspection-address-type"))),
          value = Value(Text(messages(s"inspection-address.type.${inspectionAddress.addressType}"))),
          actions = changeInspectionAddressTypeAction
        ),
        SummaryListRow(
          key = Key(Text(messages(s"$key.inspection-address"))),
          value = Value(HtmlContent(HtmlFormat.fill(addressData))),
          actions = changeInspectionAddressAction
        )
      )
    )
  }
}
