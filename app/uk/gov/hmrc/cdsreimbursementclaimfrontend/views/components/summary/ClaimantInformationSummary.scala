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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

object ClaimantInformationSummary {

  def apply(
    claimantInformation: ClaimantInformation,
    key: String,
    changeContactDetailsCall: Call,
    changeContactAddressCall: Call
  )(implicit
    messages: Messages
  ): SummaryList = {
    val contactInformation = claimantInformation.contactInformation
    val contactData        = List(
      Some(Paragraph(contactInformation.contactPerson.getOrElse(claimantInformation.fullName))),
      Some(Paragraph(contactInformation.emailAddress.getOrElse(""))),
      contactInformation.telephoneNumber.map(n => Paragraph(n))
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
      contactInformation.addressLine1.map(Paragraph(_)),
      contactInformation.addressLine2.map(Paragraph(_)),
      contactInformation.addressLine3.map(Paragraph(_)),
      contactInformation.city.map(Paragraph(_)),
      contactInformation.postalCode.map(Paragraph(_)),
      contactInformation.countryCode.map(Paragraph(_))
    ).flattenOption

    val addressAction = Some(
      Actions(
        items = Seq(
          ActionItem(
            href = s"${changeContactAddressCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages(s"$key.change-hint.address"))
          )
        )
      )
    )

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.contact-details"))),
          value = Value(HtmlContent(HtmlFormat.fill(contactData))),
          actions = contactAction
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.contact-address"))),
          value = Value(HtmlContent(HtmlFormat.fill(addressData))),
          actions = addressAction
        )
      )
    )
  }
}
