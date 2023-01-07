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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import cats.Eq
import cats.implicits.catsSyntaxEq
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.StringUtils._

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
      None,
      addressLine3.getOrElse(""),
      postalCode.getOrElse(""),
      Country(countryCode)
    )
}

object EstablishmentAddress {

  implicit val format: OFormat[EstablishmentAddress] =
    derived.oformat[EstablishmentAddress]()

  implicit val eq: Eq[EstablishmentAddress] = Eq.fromUniversalEquals

  private def combineAddressLines(line2: Option[String], line3: Option[String]): Option[String] = {
    val lines = List(line2, line3).flatten(Option.option2Iterable)
    if (lines.length === 0) None else Some(lines.mkString(", "))
  }

  def fromContactAddress(contactAddress: ContactAddress): EstablishmentAddress =
    EstablishmentAddress(
      contactAddress.line1,
      combineAddressLines(contactAddress.line2, contactAddress.line3),
      contactAddress.line4.asSomeIfNonEmpty,
      contactAddress.postcode.asSomeIfNonEmpty,
      contactAddress.country.code
    )
}
