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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address._

final case class EstablishmentAddress(
  addressLine1: String,
  addressLine2: Option[String] = None,
  addressLine3: Option[String] = None,
  postalCode: Option[String] = None,
  countryCode: String
) {
  def toContactAddress: ContactAddress =
    ContactAddress(
      addressLine1,
      addressLine2,
      addressLine3,
      "",
      postalCode.getOrElse(""),
      Country(countryCode)
    )
}

object EstablishmentAddress {

  implicit val format: OFormat[EstablishmentAddress] =
    derived.oformat[EstablishmentAddress]()

  implicit val eq: Eq[EstablishmentAddress] = Eq.fromUniversalEquals

  def fromContactAddress(contactAddress: ContactAddress): EstablishmentAddress =
    EstablishmentAddress(
      contactAddress.line1,
      contactAddress.line2,
      contactAddress.line3,
      Some(contactAddress.postcode),
      contactAddress.country.code
    )
}
