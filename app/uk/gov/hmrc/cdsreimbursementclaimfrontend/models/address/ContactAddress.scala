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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address

import cats.Eq
import julienrf.json.derived
import play.api.data.Forms.{nonEmptyText, number, of, optional, mapping => formMapping}
import play.api.data.validation._
import play.api.data.{Form, Mapping}
import play.api.i18n.Messages
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BooleanFormatter

final case class ContactAddress(
  line1: String,
  line2: Option[String],
  line3: Option[String],
  line4: String,
  postcode: String,
  country: Country
)

object ContactAddress {

  val addressLineAllowedCharacters: List[Char] =
    ('A' to 'Z').toList ::: ('a' to 'z').toList ::: ('0' to '9').toList :::
      List(' ', '-', ',', '.', '&', '\'')

  val addressLineMaxLength: Int = 35

  implicit val addressFormat: OFormat[ContactAddress] = derived.oformat[ContactAddress]()

  implicit val eq: Eq[ContactAddress] = Eq.fromUniversalEquals

  // address is selected by the index of the address in the given list
  def addressSelectForm(addresses: List[ContactAddress]): Form[ContactAddress] =
    Form(
      formMapping(
        "address-select" -> number
          .verifying("invalid", i => i >= 0 && i < addresses.size)
          .transform[ContactAddress](addresses.apply, addresses.indexOf(_))
      )(identity)(Some(_))
    )

  val addressLineMapping: Mapping[String] = {

    def validateAddressLine(s: String): ValidationResult =
      if (s.length > addressLineMaxLength) Invalid("error.tooLong")
      else if (!s.forall(addressLineAllowedCharacters.contains(_))) Invalid("error.pattern")
      else Valid

    nonEmptyText
      .transform[String](_.trim, identity)
      .verifying(Constraint[String](validateAddressLine(_)))
  }

  val addressFormMapping: Mapping[ContactAddress] =
    formMapping(
      "address-line1" -> addressLineMapping,
      "address-line2" -> optional(addressLineMapping),
      "address-line3" -> optional(addressLineMapping),
      "address-line4" -> addressLineMapping,
      "postcode"      -> Postcode.mapping,
      "countryCode"   -> of(Country.formatter)
    )(ContactAddress.apply)(ContactAddress.unapply)

  val isUkForm: Form[Boolean] =
    Form(
      formMapping(
        "isUk" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  implicit class AddressOps(private val a: ContactAddress) extends AnyVal {
    def getAddressLines(implicit messages: Messages): List[String] = {
      val lines =
        List(
          Some(a.line1),
          a.line2,
          a.line3,
          Some(a.line4),
          Some(a.postcode),
          messages.translate(s"country.${a.country.code}", Seq.empty)
        )

      lines.collect { case Some(s) => s }
    }
  }

}
