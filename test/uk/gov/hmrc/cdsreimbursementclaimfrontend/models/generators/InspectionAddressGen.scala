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

import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.{genCountry, genPostcode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{InspectionAddress, InspectionAddressType}

object InspectionAddressGen {

  lazy val genInspectionAddress = for
    num         <- Gen.choose(1, 100)
    street      <- genStringWithMaxSizeOfN(7)
    district    <- genStringWithMaxSizeOfN(5)
    city        <- genStringWithMaxSizeOfN(10)
    postcode    <- genPostcode
    country     <- genCountry
    addressType <- Gen.oneOf(InspectionAddressType.values)
  yield InspectionAddress(
    addressLine1 = Some(s"$num $street"),
    addressLine2 = Some(district),
    addressLine3 = None,
    city = Some(city),
    countryCode = Some(country.code),
    postalCode = Some(postcode),
    addressType = addressType
  )

  implicit lazy val arbitraryInspectionAddress: Arbitrary[InspectionAddress] =
    Arbitrary(genInspectionAddress)

}
