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

import cats.implicits.toFunctorFilterOps
import play.api.i18n.Messages
import play.api.mvc.Call
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

object ContactAndAddressSummary
    extends AnswerSummary[
      (
        MrnContactDetails,
        ContactAddress,
        Call,
        Option[Call]
      )
    ] {

  override def render(
    answer: (MrnContactDetails, ContactAddress, Call, Option[Call]),
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {
    val (contactDetails, contactAddress, changeContactDetailsCall, maybeChangeContactAddressCall) = answer
    val contactData                                                                               = List(
      Some(Paragraph(contactDetails.fullName)),
      Some(Paragraph(contactDetails.emailAddress.value)),
      contactDetails.phoneNumber.map(n => Paragraph(n.value))
    ).flattenOption

    val contactAction = Some(
      Actions(
        items = Seq(
          ActionItem(
            href = s"${changeContactDetailsCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.change-hint.contact"))
          )
        )
      )
    )

    val addressData = List(
      Some(Paragraph(contactAddress.line1)),
      contactAddress.line2.map(Paragraph(_)),
      contactAddress.line3.map(Paragraph(_)),
      Some(Paragraph(contactAddress.line4)),
      Some(Paragraph(contactAddress.postcode)),
      Some(Paragraph(messages(s"country.${contactAddress.country.code}")))
    ).flattenOption

    val addressAction = maybeChangeContactAddressCall.map { changeContactAddressCall =>
      Actions(
        items = Seq(
          ActionItem(
            href = s"${changeContactAddressCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.change-hint.address"))
          )
        )
      )
    }

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.contact.details"))),
          value = Value(HtmlContent(HtmlFormat.fill(contactData))),
          actions = contactAction
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.contact.address"))),
          value = Value(HtmlContent(HtmlFormat.fill(addressData))),
          actions = addressAction
        )
      )
    )
  }
}
