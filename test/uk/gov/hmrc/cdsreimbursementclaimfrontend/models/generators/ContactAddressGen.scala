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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.magnolia.Typeclass
import org.scalacheck.magnolia.gen
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country

object ContactAddressGen {

  lazy val genPostcode: Gen[String] = for {
    first <- Gen.listOfN(3, Gen.alphaNumChar)
    last  <- Gen.listOfN(3, Gen.alphaNumChar)
  } yield s"${first.mkString("")} ${last.mkString("")}"

  lazy val genContactAddressOpt: Gen[Option[ContactAddress]] =
    Gen.option(arbitraryContactAddress.arbitrary)

  lazy val genContactAddress = arbitraryContactAddress.arbitrary

  lazy val genCountry: Gen[Country] =
    Gen.oneOf("GB", "LV", "SE", "DE", "NL", "IR", "NO", "DM").map(Country(_))

  implicit lazy val arbitraryContactAddress: Typeclass[ContactAddress] =
    Arbitrary(
      for {
        num      <- Gen.choose(1, 100)
        street   <- genStringWithMaxSizeOfN(7)
        district <- Gen.option(genStringWithMaxSizeOfN(5))
        road     <- if (district.isDefined) Gen.option(genStringWithMaxSizeOfN(5)) else Gen.const(None)
        town     <- genStringWithMaxSizeOfN(10)
        postcode <- genPostcode
        country  <- genCountry
      } yield ContactAddress(
        line1 = s"$num $street",
        line2 = district,
        line3 = road,
        line4 = town,
        postcode = postcode,
        country = country
      )
    )

  implicit lazy val arbitraryAddressRequest: Typeclass[AddressLookupRequest] =
    gen[AddressLookupRequest]
}
