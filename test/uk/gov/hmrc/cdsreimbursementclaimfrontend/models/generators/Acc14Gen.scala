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

import cats.Functor
import cats.Id
import org.scalacheck.magnolia._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genCountry
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaCharGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen.genUkPhoneNumber

object Acc14Gen {

  lazy val genNdrcDetails: Gen[NdrcDetails] = for {
    taxType          <- Gen.oneOf(TaxCodes.all).map(_.value)
    amount           <- Gen.choose(0L, 10000L).map(_.toString)
    paymentMethod    <- Gen.oneOf("001", "002", "003") //001 = Immediate Payment, 002 = Duty Deferment, 003 = Cash Account
    paymentReference <- genStringWithMaxSizeOfN(18)
    cmaEligible      <- Gen.oneOf(None, Some("0"), Some("1")) //0 = CMA Not Eligible, 1 = CMA Eligible
  } yield NdrcDetails(taxType, amount, paymentMethod, paymentReference, cmaEligible)

  implicit lazy val arbitraryNdrcDetails: Typeclass[NdrcDetails] =
    Arbitrary(genNdrcDetails)

  def genListNdrcDetails(min: Int = 2, max: Int = 5): Gen[List[NdrcDetails]] = for {
    n <- Gen.choose(min, max)
    m <- Gen.listOfN(n, genNdrcDetails)
  } yield m

  lazy val genContactDetails: Gen[ContactDetails] =
    for {
      contactName  <- genStringWithMaxSizeOfN(7)
      addressLine1 <- Gen.posNum[Int].flatMap(num => genStringWithMaxSizeOfN(7).map(s => s"$num $s"))
      addressLine2 <- Gen.option(genStringWithMaxSizeOfN(10))
      addressLine3 <- Gen.option(genStringWithMaxSizeOfN(10))
      addressLine4 <- Gen.option(genStringWithMaxSizeOfN(10))
      postalCode   <- genPostcode
      countryCode  <- Gen.option(genCountry.map(_.code))
      telephone    <- Gen.option(genUkPhoneNumber.map(_.value))
      emailAddress <- genEmail.map(_.value)
    } yield ContactDetails(
      Some(contactName),
      Some(addressLine1),
      addressLine2,
      addressLine3,
      addressLine4,
      Some(postalCode),
      countryCode,
      telephone,
      Some(emailAddress)
    )

  implicit lazy val arbitraryContactDetails: Typeclass[ContactDetails] =
    Arbitrary(genContactDetails)

  lazy val genEstablishmentAddress: Gen[EstablishmentAddress] =
    for {
      num          <- Gen.choose(1, 100)
      street       <- genStringWithMaxSizeOfN(7)
      addressLine2 <- Gen.option(genStringWithMaxSizeOfN(10))
      addressLine3 <- Gen.option(genStringWithMaxSizeOfN(20))
      postalCode   <- Gen.option(genPostcode)
      countryCode  <- genCountry
    } yield EstablishmentAddress(
      s"$num $street",
      addressLine2,
      addressLine3,
      postalCode,
      countryCode.code
    )

  implicit lazy val arbitraryEstablishmentAddress: Typeclass[EstablishmentAddress] =
    Arbitrary(genEstablishmentAddress)

  lazy val genAcc14WithAddresses: DisplayDeclaration = {
    val contactDetails       =
      sample[ContactDetails].copy(
        contactName = Some(alphaCharGen(20)),
        addressLine1 = Some(alphaCharGen(20)),
        addressLine2 = Some(alphaCharGen(20)),
        addressLine3 = Some(alphaCharGen(20)),
        addressLine4 = Some(alphaCharGen(20)),
        postalCode = Some(alphaCharGen(7)),
        countryCode = Some("GB"),
        telephone = Some(sample[PhoneNumber].value)
      )
    val establishmentAddress = sample[EstablishmentAddress]
      .copy(
        addressLine2 = Some(alphaCharGen(20)),
        addressLine3 = Some(alphaCharGen(20)),
        postalCode = Some(alphaCharGen(6)),
        countryCode = "GB"
      )
    val consignee            = sample[ConsigneeDetails]
      .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))

    val declarant = sample[DeclarantDetails]
      .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))

    val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
      dd.copy(displayResponseDetail =
        dd.displayResponseDetail.copy(
          consigneeDetails = Some(consignee),
          declarantDetails = declarant
        )
      )
    )

    displayDeclaration
  }

  lazy val genAcc14WithoutContactDetails: DisplayDeclaration = {
    val establishmentAddress = sample[EstablishmentAddress]
      .copy(
        addressLine2 = Some(alphaCharGen(20)),
        addressLine3 = Some(alphaCharGen(20)),
        postalCode = Some(alphaCharGen(6)),
        countryCode = "GB"
      )
    val consignee            = sample[ConsigneeDetails]
      .copy(establishmentAddress = establishmentAddress, contactDetails = None)

    val declarant = sample[DeclarantDetails]
      .copy(establishmentAddress = establishmentAddress, contactDetails = None)

    val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
      dd.copy(displayResponseDetail =
        dd.displayResponseDetail.copy(
          consigneeDetails = Some(consignee),
          declarantDetails = declarant
        )
      )
    )

    displayDeclaration
  }

  lazy val genAcc14WithoutConsigneeAndDeclarantDetails: DisplayDeclaration = {
    val establishmentAddress = sample[EstablishmentAddress]
      .copy(
        addressLine2 = Some(alphaCharGen(20)),
        addressLine3 = Some(alphaCharGen(20)),
        postalCode = Some(alphaCharGen(6)),
        countryCode = "GB"
      )

    val declarant = sample[DeclarantDetails]
      .copy(establishmentAddress = establishmentAddress, contactDetails = None)

    val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
      dd.copy(displayResponseDetail =
        dd.displayResponseDetail.copy(
          consigneeDetails = None,
          declarantDetails = declarant
        )
      )
    )

    displayDeclaration
  }
}
