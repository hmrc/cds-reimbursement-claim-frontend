/*
 * Copyright 2021 HM Revenue & Customs
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
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.{ActionItem, Text}
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class ContactDetailsSummary extends AnswerSummary[(MrnContactDetails, ContactAddress)] {

  def render(key: String, answer: (MrnContactDetails, ContactAddress))(implicit
    journey: JourneyBindable,
    messages: Messages
  ): SummaryList = {

    def renderContactDetails(contactDetails: MrnContactDetails): SummaryListRow = {
      val data = List(
        Some(Paragraph(contactDetails.fullName)),
        Some(Paragraph(contactDetails.emailAddress.value)),
        contactDetails.phoneNumber.map(n => Paragraph(n.value))
      ).flattenOption

      SummaryListRow(
        key = Key(Text(messages(s"$key.contact.details"))),
        value = Value(HtmlContent(HtmlFormat.fill(data))),
        actions = Some(
          Actions(
            "govuk-link",
            List(
              ActionItem(
                href = s"${routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.contact.details"))
              )
            )
          )
        )
      )
    }

    def renderContactAddress(contactAddress: ContactAddress): SummaryListRow = {
      val data = List(
        Some(Paragraph(contactAddress.line1)),
        contactAddress.line2.map(Paragraph(_)),
        contactAddress.line3.map(Paragraph(_)),
        Some(Paragraph(contactAddress.line4)),
        Some(Paragraph(contactAddress.postcode)),
        Some(Paragraph(messages(contactAddress.country.messageKey)))
      ).flattenOption

      SummaryListRow(
        key = Key(Text(messages(s"$key.contact.address"))),
        value = Value(HtmlContent(HtmlFormat.fill(data))),
        actions = Some(
          Actions(
            "govuk-link",
            List(
              ActionItem(
                href = s"${routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.contact.address"))
              )
            )
          )
        )
      )

    }

    SummaryList(
      Seq(
        renderContactDetails(answer._1),
        renderContactAddress(answer._2)
      )
    )
  }
}
