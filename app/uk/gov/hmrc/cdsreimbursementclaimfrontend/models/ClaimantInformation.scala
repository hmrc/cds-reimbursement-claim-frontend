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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeOrDeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.StringUtils._

/** Comprehensive information about user filling out the claim. */
final case class ClaimantInformation(
  eori: Eori,
  fullName: String,
  establishmentAddress: ContactInformation,
  contactInformation: ContactInformation
) {

  def summaryContact(sep: String): String = Seq(
    Some(contactInformation.contactPerson.getOrElse(fullName)),
    contactInformation.emailAddress,
    contactInformation.telephoneNumber
  )
    .collect { case Some(x) => x }
    .mkString(sep)

  def summaryAddress(sep: String): String =
    Seq(
      contactInformation.addressLine1,
      contactInformation.addressLine2,
      contactInformation.addressLine3,
      contactInformation.postalCode,
      contactInformation.countryCode
    )
      .collect { case Some(x) => x }
      .mkString(sep)
}

object ClaimantInformation {

  def from(
    claimantEoriNumber: Eori,
    claimantDetails: Option[ConsigneeOrDeclarantDetails],
    contactDetails: MrnContactDetails,
    contactAddress: ContactAddress
  ): ClaimantInformation =
    ClaimantInformation(
      eori = claimantEoriNumber,
      fullName = claimantDetails.map(_.legalName).getOrElse(""),
      establishmentAddress = ContactInformation(
        contactPerson = claimantDetails.map(_.legalName),
        addressLine1 = claimantDetails.map(_.establishmentAddress.addressLine1),
        addressLine2 = claimantDetails.flatMap(_.establishmentAddress.addressLine2),
        addressLine3 = None,
        street = concat(
          claimantDetails.map(_.establishmentAddress.addressLine1),
          claimantDetails.flatMap(_.establishmentAddress.addressLine2)
        ),
        city = claimantDetails.flatMap(_.establishmentAddress.addressLine3),
        postalCode = claimantDetails.flatMap(_.establishmentAddress.postalCode),
        countryCode = claimantDetails.map(_.establishmentAddress.countryCode),
        telephoneNumber = None,
        faxNumber = None,
        emailAddress = contactDetails.emailAddress.value.asSomeIfNonEmpty
      ),
      contactInformation = ContactInformation(
        contactPerson = contactDetails.fullName.asSomeIfNonEmpty,
        addressLine1 = contactAddress.line1.asSomeIfNonEmpty,
        addressLine2 = contactAddress.line2,
        addressLine3 = contactAddress.line3,
        street = concat(contactAddress.line1.asSomeIfNonEmpty, contactAddress.line2),
        city = contactAddress.line4.asSomeIfNonEmpty,
        countryCode = contactAddress.country.code.asSomeIfNonEmpty,
        postalCode = contactAddress.postcode.asSomeIfNonEmpty,
        telephoneNumber = contactDetails.phoneNumber.map(_.value),
        faxNumber = None,
        emailAddress = contactDetails.emailAddress.value.asSomeIfNonEmpty
      )
    )

  private def concat(lineA: Option[String], lineB: Option[String]): Option[String] =
    (lineA, lineB) match {
      case (Some(s1), Some(s2)) => Some(s"$s1 $s2")
      case (Some(s1), None)     => Some(s1)
      case (None, Some(s2))     => Some(s2)
      case _                    => Some("")
    }

  implicit val eq: Eq[ClaimantInformation]         = Eq.fromUniversalEquals[ClaimantInformation]
  implicit val format: Format[ClaimantInformation] = Json.format[ClaimantInformation]
}
