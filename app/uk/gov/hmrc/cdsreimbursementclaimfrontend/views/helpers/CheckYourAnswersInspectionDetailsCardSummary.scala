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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.routes as rejectedGoodsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.routes as rejectedGoodsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes as rejectedGoodsSingleRoutes
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object CheckYourAnswersInspectionDetailsCardSummary {

  private def render(
    inspectionDate: InspectionDate,
    inspectionAddressType: InspectionAddressType,
    inspectionAddress: InspectionAddress,
    inspectionDateChangeCallOpt: Option[Call],
    inspectionAddressTypeChangeCallOpt: Option[Call],
    inspectionAddressChangeCallOpt: Option[Call]
  )(implicit messages: Messages): SummaryList =
    SummaryList(
      Seq(
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.inspection.inspection-date"))),
            value = Value(Text(toDisplayDate(InspectionDate(inspectionDate.value).checkYourDetailsDisplayFormat))),
            actions = inspectionDateChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${changeCall.url}",
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.inspection.inspection-date"))
                  )
                )
              )
            )
          )
        ),
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.inspection.inspection-address"))),
            value = Value(HtmlContent(HtmlFormat.fill(getInspectionAddressData(inspectionAddress)))),
            actions = inspectionAddressTypeChangeCallOpt
              .map(changeCall =>
                Actions(
                  items = Seq(
                    ActionItem(
                      href = s"${changeCall.url}",
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages("check-your-answers.inspection.inspection-address"))
                    )
                  )
                )
              )
          )
        ),
        inspectionAddressType match {
          case InspectionAddressType.Other =>
            Some(
              SummaryListRow(
                key = Key(HtmlContent(messages("check-your-answers.inspection.address"))),
                value = Value(HtmlContent(HtmlFormat.fill(getAddressData(inspectionAddress)))),
                actions = inspectionAddressChangeCallOpt
                  .map(changeCall =>
                    Actions(
                      items = Seq(
                        ActionItem(
                          href = s"${changeCall.url}",
                          content = Text(messages("cya.change")),
                          visuallyHiddenText = Some(messages("check-your-answers.inspection.address"))
                        )
                      )
                    )
                  )
              )
            )
          case _                           => None
        }
      ).flatten
    )

  private def getAddressData(inspectionAddress: InspectionAddress)(implicit
    messages: Messages
  ): List[HtmlFormat.Appendable] = List(
    inspectionAddress.addressLine1.map(Paragraph(_)),
    inspectionAddress.addressLine2.map(Paragraph(_)),
    inspectionAddress.addressLine3.map(Paragraph(_)),
    inspectionAddress.city.map(Paragraph(_)),
    inspectionAddress.postalCode.map(Paragraph(_)),
    inspectionAddress.countryCode.map(code => messages(s"country.$code")).map(Paragraph(_))
  ).flatten

  private def getInspectionAddressData(
    inspectionAddress: InspectionAddress
  )(implicit messages: Messages): List[HtmlFormat.Appendable] =
    inspectionAddress.addressType match {
      case InspectionAddressType.Other =>
        List(Paragraph(messages(s"inspection-address.type.${inspectionAddress.addressType}")))
      case _                           =>
        List(Paragraph(s"${messages(s"inspection-address.type.${inspectionAddress.addressType}")}:")) ++
          getAddressData(inspectionAddress)
    }

  def renderForSingle(claim: RejectedGoodsSingleJourney.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      inspectionDate = claim.inspectionDate,
      inspectionAddressType = claim.inspectionAddress.addressType,
      inspectionAddress = claim.inspectionAddress,
      inspectionDateChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsSingleRoutes.EnterInspectionDateController.show) else None,
      inspectionAddressTypeChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsSingleRoutes.ChooseInspectionAddressTypeController.show) else None,
      inspectionAddressChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsSingleRoutes.ChooseInspectionAddressTypeController.redirectToALF())
        else None
    )

  def renderForMultiple(claim: RejectedGoodsMultipleJourney.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      inspectionDate = claim.inspectionDate,
      inspectionAddressType = claim.inspectionAddress.addressType,
      inspectionAddress = claim.inspectionAddress,
      inspectionDateChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsMultipleRoutes.EnterInspectionDateController.show) else None,
      inspectionAddressTypeChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsMultipleRoutes.ChooseInspectionAddressTypeController.show) else None,
      inspectionAddressChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsMultipleRoutes.ChooseInspectionAddressTypeController.redirectToALF())
        else None
    )

  def renderForScheduled(claim: RejectedGoodsScheduledJourney.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      inspectionDate = claim.inspectionDate,
      inspectionAddressType = claim.inspectionAddress.addressType,
      inspectionAddress = claim.inspectionAddress,
      inspectionDateChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.EnterInspectionDateController.show) else None,
      inspectionAddressTypeChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.ChooseInspectionAddressTypeController.show) else None,
      inspectionAddressChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.ChooseInspectionAddressTypeController.redirectToALF())
        else None
    )
}
