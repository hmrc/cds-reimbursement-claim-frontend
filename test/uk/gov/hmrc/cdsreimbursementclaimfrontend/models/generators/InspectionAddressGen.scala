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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.magnolia.Typeclass
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType

object InspectionAddressGen {

  lazy val genInspectionAddress = for {
    num         <- Gen.choose(1, 100)
    street      <- genStringWithMaxSizeOfN(7)
    district    <- Gen.option(genStringWithMaxSizeOfN(5))
    road        <- if (district.isDefined) Gen.option(genStringWithMaxSizeOfN(5))
                   else Gen.const(None)
    city        <- genStringWithMaxSizeOfN(10)
    postcode    <- genPostcode
    addressType <- Gen.oneOf(InspectionAddressType.values)
  } yield InspectionAddress(
    addressLine1 = s"$num $street",
    addressLine2 = district,
    addressLine3 = road,
    city = Some(city),
    postalCode = postcode,
    addressType = addressType
  )

  implicit lazy val arbitraryInspectionAddress: Typeclass[InspectionAddress] =
    Arbitrary(genInspectionAddress)

}
