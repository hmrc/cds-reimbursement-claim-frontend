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
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.StringUtils._

final case class InspectionAddress(
  addressLine1: String,
  addressLine2: String,
  city: String,
  countryCode: String,
  postalCode: String,
  addressType: InspectionAddressType
) {
  def summaryAddress(sep: String): String =
    Seq(addressLine1, addressLine2, city, postalCode).mkString(sep)
}

object InspectionAddress {

  def ofType(inspectionAddressType: InspectionAddressType): Builder =
    Builder(inspectionAddressType)

  final case class Builder(inspectionAddressType: InspectionAddressType) extends AnyVal {

    def mapFrom(contactAddress: ContactAddress): InspectionAddress =
      InspectionAddress(
        addressLine1 = contactAddress.line1,
        addressLine2 = contactAddress.line2.getOrElse(" "),
        city = contactAddress.line4.asSomeIfNonEmpty.orElse(contactAddress.line3).getOrElse(""),
        countryCode = contactAddress.country.code,
        postalCode = contactAddress.postcode,
        addressType = inspectionAddressType
      )

    def mapFrom(contactDetails: ContactDetails): InspectionAddress =
      InspectionAddress(
        addressLine1 = contactDetails.addressLine1.getOrElse(""),
        addressLine2 = contactDetails.addressLine2.getOrElse(" "),
        city = contactDetails.addressLine4.orElse(contactDetails.addressLine3).getOrElse(""),
        countryCode = contactDetails.countryCode.getOrElse(""),
        postalCode = contactDetails.postalCode.getOrElse(""),
        addressType = inspectionAddressType
      )
  }

  implicit val equality: Eq[InspectionAddress] =
    Eq.fromUniversalEquals[InspectionAddress]

  implicit val format: OFormat[InspectionAddress] =
    derived.oformat[InspectionAddress]()
}
