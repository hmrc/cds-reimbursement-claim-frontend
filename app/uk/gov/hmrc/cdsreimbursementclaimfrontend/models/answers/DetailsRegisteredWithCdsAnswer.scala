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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

final case class DetailsRegisteredWithCdsAnswer(
  fullName: String,
  emailAddress: Email,
  contactAddress: ContactAddress,
  addCompanyDetails: Boolean
)

object DetailsRegisteredWithCdsAnswer {

  def apply(
    declarant: DeclarantTypeAnswer,
    declaration: DisplayDeclaration,
    email: Email
  ): DetailsRegisteredWithCdsAnswer = {

    def consigneeToClaimantDetails(displayDeclaration: DisplayDeclaration, verifiedEmail: Email) = {
      val declaration          = displayDeclaration.displayResponseDetail
      val establishmentAddress = declaration.consigneeDetails.map(p => p.establishmentAddress)
      DetailsRegisteredWithCdsAnswer(
        declaration.consigneeDetails.map(_.legalName).getOrElse(""),
        verifiedEmail,
        ContactAddress(
          establishmentAddress.map(_.addressLine1).getOrElse(""),
          establishmentAddress.flatMap(_.addressLine2),
          None,
          establishmentAddress.flatMap(_.addressLine3).getOrElse(""),
          establishmentAddress.flatMap(_.postalCode).getOrElse(""),
          establishmentAddress.map(cc => Country(cc.countryCode)).getOrElse(Country.uk)
        ),
        addCompanyDetails = false
      )
    }

    def declarantToClaimantDetails(displayDeclaration: DisplayDeclaration, verifiedEmail: Email) = {
      val declaration          = displayDeclaration.displayResponseDetail
      val establishmentAddress = declaration.declarantDetails.establishmentAddress

      DetailsRegisteredWithCdsAnswer(
        declaration.declarantDetails.legalName,
        verifiedEmail,
        ContactAddress(
          establishmentAddress.addressLine1,
          establishmentAddress.addressLine2,
          None,
          establishmentAddress.addressLine3.getOrElse(""),
          establishmentAddress.postalCode.getOrElse(""),
          Country(establishmentAddress.countryCode)
        ),
        addCompanyDetails = false
      )
    }

    declarant match {
      case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
        consigneeToClaimantDetails(declaration, email)
      case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
        declarantToClaimantDetails(declaration, email)
    }
  }

  implicit val cdsDetailsFormat: OFormat[DetailsRegisteredWithCdsAnswer] =
    Json.format[DetailsRegisteredWithCdsAnswer]
}
