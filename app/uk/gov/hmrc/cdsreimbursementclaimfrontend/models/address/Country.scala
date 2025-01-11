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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address

import cats.Eq
import cats.syntax.eq.*
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country.CountryCode

import scala.io.Codec
import scala.io.Source

final case class Country(
  code: CountryCode
) {
  val messageKey: String = s"country.$code"
}

object Country {

  val uk: Country = Country("GB")

  type CountryCode = String

  val countryCodes: List[CountryCode] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream("/resources/countries.txt"))(Codec.UTF8)
    try source.getLines().toList
    finally source.close()
  }

  implicit class CountryOps(private val c: Country) {
    def isUk: Boolean = c === Country.uk
  }

  implicit val countryFormat: OFormat[Country] = Json.format[Country]

  implicit val eq: Eq[Country] = Eq.fromUniversalEquals
}
