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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.optional
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

object Forms {
  def eoriNumberForm(key: String): Form[Eori] = Form(
    mapping(
      key -> nonEmptyText(maxLength = 18)
        .verifying("invalid.number", str => str.length > 18 || str.isEmpty || Eori(str).isValid)
    )(Eori.apply)(Eori.unapply)
  )

  val basisOfRejectedGoodsClaimForm: Form[BasisOfRejectedGoodsClaim] = Form(
    mapping(
      "select-basis-for-claim.rejected-goods" -> nonEmptyText
        .verifying("error.required", basis => basis.isEmpty || BasisOfRejectedGoodsClaim.has(basis))
    )(BasisOfRejectedGoodsClaim.findUnsafe)(borgc => Option(borgc.toString))
  )

  val mrnContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-contact-details.contact-phone-number" -> optional(PhoneNumber.mapping)
    )(MrnContactDetails.apply)(MrnContactDetails.unapply)
  )

  val rejectedGoodsContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-contact-details-rejected-goods.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-contact-details-rejected-goods.contact-email"        -> Email.mappingMaxLength,
      "enter-contact-details-rejected-goods.contact-phone-number" -> optional(PhoneNumber.mapping)
    )(MrnContactDetails.apply)(MrnContactDetails.unapply)
  )

  val methodOfDisposalForm: Form[MethodOfDisposal] =
    Form(
      mapping(
        "select-method-of-disposal.rejected-goods" -> nonEmptyText
          .verifying(MethodOfDisposal.values.map(keyOf).contains _)
          .transform[MethodOfDisposal](
            {
              case "Export"                   => Export
              case "PostalExport"             => PostalExport
              case "DonationToCharity"        => DonationToCharity
              case "PlacedInCustomsWarehouse" => PlacedInCustomsWarehouse
              case "ExportInBaggage"          => ExportInBaggage
              case "Destruction"              => Destruction
            },
            _.toString
          )
      )(identity)(Some(_))
    )
}
