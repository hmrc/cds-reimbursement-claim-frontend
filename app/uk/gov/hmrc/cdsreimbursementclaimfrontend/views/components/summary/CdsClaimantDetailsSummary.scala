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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.NamePhoneEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class CdsClaimantDetailsSummary
    extends AnswerSummary[
      (
        NamePhoneEmail,
        EstablishmentAddress,
        Option[MrnContactDetails],
        Option[ContactAddress]
      )
    ] {

  def render(
    key: String,
    answer: (NamePhoneEmail, EstablishmentAddress, Option[MrnContactDetails], Option[ContactAddress])
  )(implicit
    router: ReimbursementRoutes,
    messages: Messages
  ): SummaryList = {
    val (cdsContact, cdsAddress, mrnContact, mrnAddress) = answer

    val includeAdditionalContacts = mrnContact.isDefined && mrnAddress.isDefined

    val additionalContactKey = if (includeAdditionalContacts) {
      "yes"
    } else {
      "no"
    }

    def renderContactDetails(contactDetails: NamePhoneEmail, isAdditional: Boolean): SummaryListRow = {
      val actions =
        if (isAdditional)
          Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.CheckContactDetailsMrnController.show(router.journeyBindable).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.change-hint.contact"))
                )
              )
            )
          )
        else None

      val data = List(
        contactDetails.name.map(Paragraph(_)),
        contactDetails.email.map(email => Paragraph(email.value)),
        contactDetails.phoneNumber.map(n => Paragraph(n.value))
      ).flattenOption

      SummaryListRow(
        key = Key(Text(messages(s"$key.${role(isAdditional)}.contact.details"))),
        value = Value(HtmlContent(HtmlFormat.fill(data))),
        actions = actions
      )
    }

    def renderEstablishmentAddress(address: EstablishmentAddress, isAdditional: Boolean): SummaryListRow = {
      val actions =
        if (isAdditional)
          Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.CheckContactDetailsMrnController.show(router.journeyBindable).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.change-hint.address"))
                )
              )
            )
          )
        else None

      val data = List(
        Some(Paragraph(address.addressLine1)),
        address.addressLine2.map(Paragraph(_)),
        address.addressLine3.map(Paragraph(_)),
        address.postalCode.map(Paragraph(_)),
        Some(Paragraph(messages(s"country.${address.countryCode}")))
      ).flattenOption

      SummaryListRow(
        key = Key(Text(messages(s"$key.${role(isAdditional)}.contact.address"))),
        value = Value(HtmlContent(HtmlFormat.fill(data))),
        actions = actions
      )
    }

    def additionalContact: SummaryListRow =
      SummaryListRow(
        key = Key(Text(messages(s"$key.contact.additional"))),
        value = Value(Text(messages(s"generic.$additionalContactKey"))),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${routes.CheckContactDetailsMrnController.show(router.journeyBindable).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.change-hint.contact"))
              )
            )
          )
        )
      )

    val maybeAdditionalContact = mrnContact.map { contact =>
      renderContactDetails(NamePhoneEmail.fromMrnContactDetails(contact), isAdditional = true)
    }

    val maybeAdditionalAddress = mrnAddress.map { address =>
      renderEstablishmentAddress(EstablishmentAddress.fromContactAddress(address), isAdditional = true)
    }

    SummaryList(
      Seq(
        Some(renderContactDetails(cdsContact, isAdditional = false)),
        Some(renderEstablishmentAddress(cdsAddress, isAdditional = false)),
        Some(additionalContact),
        maybeAdditionalContact,
        maybeAdditionalAddress
      ).flatten(Option.option2Iterable)
    )
  }

  private def role(isAdditional: Boolean): String = if (isAdditional) "additional" else "cds"
}
