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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country

import java.util.Locale
import scala.collection.immutable.ArraySeq

object ContactAddressGen {

  lazy val genPostcode: Gen[String] = for {
    first <- Gen.listOfN(3, Gen.alphaNumChar)
    last  <- Gen.listOfN(3, Gen.alphaNumChar)
  } yield s"${first.mkString("")} ${last.mkString("")}"

  lazy val genContactAddressOpt: Gen[Option[ContactAddress]] =
    Gen.option(arbitraryContactAddress.arbitrary)

  lazy val genContactAddress: Gen[ContactAddress] =
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

  lazy val genCountry: Gen[Country] = Gen
    .oneOf(ArraySeq.unsafeWrapArray(Locale.getISOCountries))
    .map(Country(_))

  implicit lazy val arbitraryContactAddress: Arbitrary[ContactAddress] =
    Arbitrary(genContactAddress)

  implicit lazy val arbitraryAddressRequest: Arbitrary[AddressLookupRequest] =
    GeneratorUtils.gen[AddressLookupRequest]
}
