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

object CheckYourAnswersContactDetailsCardSummary {

  def apply(
    claimantInformation: ClaimantInformation
  )(implicit
    messages: Messages
  ): SummaryList =
    buildSummaryList(getContactDataHtml(claimantInformation), getAddressDataHtml(claimantInformation))

  def apply(
    claimantInformation: ClaimantInformation,
    changeContactDetailsCall: Option[Call],
    changeContactAddressCall: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {

    val contactData = getContactDataHtml(claimantInformation)

    val contactAction = changeContactDetailsCall.map { contactDetailsCall =>
      Actions(
        items = Seq(
          ActionItem(
            href = s"${contactDetailsCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages("check-your-answers.contact-information.personal-details"))
          )
        )
      )
    }

    val addressData = getAddressDataHtml(claimantInformation)

    val addressAction = changeContactAddressCall.map { addressCall =>
      Actions(
        items = Seq(
          ActionItem(
            href = s"${addressCall.url}",
            content = Text(messages("cya.change")),
            visuallyHiddenText = Some(messages("check-your-answers.contact-information.address"))
          )
        )
      )
    }

    buildSummaryList(contactData, addressData, contactAction, addressAction)
  }

  private def buildSummaryList(
    contactData: List[HtmlFormat.Appendable],
    addressData: List[HtmlFormat.Appendable],
    contactAction: Option[Actions] = None,
    addressAction: Option[Actions] = None
  )(implicit
    messages: Messages
  ) =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.contact-information.personal-details"))),
          value = Value(HtmlContent(HtmlFormat.fill(contactData))),
          actions = contactAction
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.contact-information.address"))),
          value = Value(HtmlContent(HtmlFormat.fill(addressData))),
          actions = addressAction
        )
      )
    )

  def getContactDataHtml(claimantInformation: ClaimantInformation): List[HtmlFormat.Appendable] = {
    val contactInformation = claimantInformation.contactInformation
    List(
      Some(Paragraph(contactInformation.contactPerson.getOrElse(claimantInformation.fullName))),
      Some(Paragraph(contactInformation.emailAddress.getOrElse(""))),
      contactInformation.telephoneNumber.map(n => Paragraph(n))
    ).flattenOption
  }

  def getContactDataString(claimantInformation: ClaimantInformation): String = {
    val contactInformation = claimantInformation.contactInformation
    List(
      Some(contactInformation.contactPerson.getOrElse(claimantInformation.fullName)),
      Some(contactInformation.emailAddress.getOrElse("")),
      contactInformation.telephoneNumber
    ).flattenOption.mkString(" ").trim
  }

  def getAddressDataHtml(
    claimantInformation: ClaimantInformation
  )(implicit messages: Messages): List[HtmlFormat.Appendable] = {
    val contactInformation = claimantInformation.contactInformation
    List(
      contactInformation.addressLine1.map(Paragraph(_)),
      contactInformation.addressLine2.map(Paragraph(_)),
      contactInformation.addressLine3.map(Paragraph(_)),
      contactInformation.city.map(Paragraph(_)),
      contactInformation.postalCode.map(Paragraph(_)),
      contactInformation.countryCode.map(code => messages(s"country.$code")).map(Paragraph(_))
    ).flattenOption
  }

  def getAddressDataString(claimantInformation: ClaimantInformation)(implicit messages: Messages): String = {
    val contactInformation = claimantInformation.contactInformation
    List(
      contactInformation.addressLine1,
      contactInformation.addressLine2,
      contactInformation.addressLine3,
      contactInformation.city,
      contactInformation.postalCode,
      contactInformation.countryCode.map(code => messages(s"country.$code"))
    ).flattenOption.mkString(" ").trim
  }
}
