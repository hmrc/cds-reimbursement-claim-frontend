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
import play.api.mvc.Call
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
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

object InspectionDateAndAddressSummary {

  def apply(
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    key: String,
    changeInspectionDateCall: Call,
    changeInspectionAddressTypeCall: Call,
    maybeChangeInspectionAddressCall: Option[Call] = None
  )(implicit
    messages: Messages
  ): SummaryList = {

    val changeInspectionDateAction =
      Actions(
        items = Seq(
          ActionItem(
            href = s"${changeInspectionDateCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.inspection-date"))
          )
        )
      )

    val changeInspectionAddressTypeAction =
      Actions(
        items = Seq(
          ActionItem(
            href = s"${changeInspectionAddressTypeCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.inspection-address-type"))
          )
        )
      )

    lazy val changeInspectionAddressAction =
      Actions(
        items = Seq(
          ActionItem(
            href = s"${maybeChangeInspectionAddressCall.map(_.url).getOrElse("")}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.inspection-address-type"))
          )
        )
      )

    val addressData = List(
      inspectionAddress.addressLine1.map(Paragraph(_)),
      inspectionAddress.addressLine2.map(Paragraph(_)),
      inspectionAddress.addressLine3.map(Paragraph(_)),
      inspectionAddress.city.map(Paragraph(_)),
      inspectionAddress.postalCode.map(Paragraph(_)),
      inspectionAddress.countryCode.map(code => Paragraph(messages(s"country.$code")))
    ).flatten(Option.option2Iterable)

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.inspection-date"))),
          value = Value(Text(InspectionDate(inspectionDate.value).checkYourDetailsDisplayFormat)),
          actions = Some(changeInspectionDateAction)
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.inspection-address-type"))),
          value = Value(Text(messages(s"inspection-address.type.${inspectionAddress.addressType}"))),
          actions = Some(changeInspectionAddressTypeAction)
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.inspection-address"))),
          value = Value(HtmlContent(HtmlFormat.fill(addressData))),
          actions = (inspectionAddress.addressType, maybeChangeInspectionAddressCall) match {
            case (Other, Some(_)) => Some(changeInspectionAddressAction)
            case (Other, None)    => Some(changeInspectionAddressTypeAction)
            case _                => None
          }
        )
      )
    )
  }
}
