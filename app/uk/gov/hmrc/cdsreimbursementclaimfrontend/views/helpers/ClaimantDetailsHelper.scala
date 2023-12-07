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

import cats.implicits._
import play.api.i18n.Messages
import play.twirl.api.Html
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.NamePhoneEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.paragraph
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class ClaimantDetailsHelper @Inject() () {

  protected val key = "claimant-details"

  def renderContactRegisteredWithCDS(namePhoneEmail: NamePhoneEmail)(implicit messages: Messages): SummaryListRow = {
    val data = List(
      namePhoneEmail.name.map(getParagraph),
      namePhoneEmail.phoneNumber.map(a => getParagraph(a.value)),
      namePhoneEmail.email.map(a => getParagraph(a.value))
    ).flattenOption

    SummaryListRow(
      Key(HtmlContent(messages(s"$key.registered.details"))),
      Value(new HtmlContent(HtmlFormat.fill(data)))
    )
  }

  def renderEstablishmentAddress(
    establishmentAddress: EstablishmentAddress
  )(implicit messages: Messages): SummaryListRow = {
    val data = List(
      getParagraph(establishmentAddress.addressLine1).some,
      establishmentAddress.addressLine2.map(getParagraph),
      establishmentAddress.addressLine3.map(getParagraph),
      establishmentAddress.postalCode.map(getParagraph)
    ).flattenOption

    SummaryListRow(
      Key(HtmlContent(messages(s"$key.registered.address"))),
      Value(new HtmlContent(HtmlFormat.fill(data)))
    )
  }

  def getParagraph(in: String): HtmlFormat.Appendable = new paragraph()(Html(in))

}
