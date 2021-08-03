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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import cats.implicits._
import play.api.i18n.{Lang, Langs, MessagesApi}
import play.twirl.api.{Html, HtmlFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{CheckClaimantDetailsController, JourneyBindable, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.NamePhoneEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.paragraph_block
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{HtmlContent, Text}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ActionItem, Actions, Key, SummaryListRow, Value}

import javax.inject.{Inject, Singleton}

@Singleton
class ClaimantDetailsHelper @Inject() (implicit langs: Langs, messages: MessagesApi, journeyBindable: JourneyBindable) {
  protected val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)
  protected val key        = CheckClaimantDetailsController.languageKey

  def renderDetailsRegisteredWithCDS(
    namePhoneEmail: NamePhoneEmail,
    establishmentAddress: Option[EstablishmentAddress]
  ): List[SummaryListRow] =
    List(
      if (namePhoneEmail.nonEmpty()) Some(renderContactRegisteredWithCDS(namePhoneEmail)) else None,
      establishmentAddress.map(renderEstablishmentAddress(_))
    ).flattenOption

  def renderContactDetails(
    contactDetails: NamePhoneEmail,
    contactAddress: Option[NonUkAddress]
  ): List[SummaryListRow] =
    List(
      if (contactDetails.nonEmpty()) Some(renderContactDetails(contactDetails)) else None,
      contactAddress.map(renderContactAddress(_))
    ).flattenOption

  def renderContactRegisteredWithCDS(namePhoneEmail: NamePhoneEmail): SummaryListRow = {
    val data = List(
      namePhoneEmail.name.map(getParagraph),
      namePhoneEmail.phoneNumber.map(a => getParagraph(a.value)),
      namePhoneEmail.email.map(a => getParagraph(a.value))
    ).flattenOption

    SummaryListRow(
      Key(Text(messages(s"$key.registered.details")(lang))),
      Value(new HtmlContent(HtmlFormat.fill(data)))
    )
  }

  def renderEstablishmentAddress(establishmentAddress: EstablishmentAddress): SummaryListRow = {
    val data = List(
      getParagraph(establishmentAddress.addressLine1).some,
      establishmentAddress.addressLine2.map(getParagraph),
      establishmentAddress.addressLine3.map(getParagraph),
      establishmentAddress.postalCode.map(getParagraph)
    ).flattenOption

    SummaryListRow(
      Key(Text(messages(s"$key.registered.address")(lang))),
      Value(new HtmlContent(HtmlFormat.fill(data)))
    )
  }

  def renderContactDetails(namePhoneEmail: NamePhoneEmail): SummaryListRow = {
    val data = List(
      namePhoneEmail.name.map(getParagraph),
      namePhoneEmail.phoneNumber.map(a => getParagraph(a.value)),
      namePhoneEmail.email.map(a => getParagraph(a.value))
    ).flattenOption

    SummaryListRow(
      Key(Text(messages(s"$key.contact.details")(lang))),
      Value(new HtmlContent(HtmlFormat.fill(data))),
      "",
      Some(
        Actions(
          "govuk-link",
          List(
            ActionItem(
              href = s"${routes.EnterOrChangeContactDetailsController.changeMrnContactDetails(journeyBindable).url}",
              Text(messages("claimant-details.change")(lang))
            )
          )
        )
      )
    )
  }

  def renderContactAddress(contactAddress: NonUkAddress): SummaryListRow = {
    val data = List(
      getParagraph(contactAddress.line1).some,
      contactAddress.line2.map(getParagraph),
      contactAddress.line3.map(getParagraph),
      getParagraph(contactAddress.line4).some,
      getParagraph(contactAddress.postcode).some,
      getParagraph(messages(contactAddress.country.messageKey)(lang)).some
    ).flattenOption

    SummaryListRow(
      Key(Text(messages(s"$key.contact.address")(lang))),
      Value(new HtmlContent(HtmlFormat.fill(data))),
      "",
      Some(Actions("govuk-link", List(ActionItem("", Text(messages("claimant-details.change")(lang))))))
    )

  }

  def getParagraph(in: String): HtmlFormat.Appendable = new paragraph_block()(Html(in), Some("govuk-body"))

}
