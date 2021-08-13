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

import cats.{Functor, Id}
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeDetails, ContactDetails, DeclarantDetails, DisplayDeclaration, EstablishmentAddress, NdrcDetails}
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{alphaCharGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber

object Acc14Gen {

  def genNdrcDetails: Gen[NdrcDetails] = for {
    taxType          <- Gen.oneOf(TaxCode.allTaxCodes).map(_.value)
    amount           <- Gen.choose(0L, 10000.toLong).map(_.toString)
    paymentMethod    <- Gen.oneOf("001", "002", "003") //001 = Immediate Payment, 002 = Duty Deferment, 003 = Cash Account
    paymentReference <- arbitraryString.arbitrary.map(_.take(18))
    cmaEligible      <- Gen.oneOf(None, Some("0"), Some("1")) //0 = CMA Not Eligible, 1 = CMA Eligible
  } yield NdrcDetails(taxType, amount, paymentMethod, paymentReference, cmaEligible)

  implicit val arbitraryNdrcDetails: Typeclass[NdrcDetails] = Arbitrary(genNdrcDetails)

  implicit val arbitraryContactDetails: Typeclass[ContactDetails] = gen[ContactDetails]

  implicit val arbitraryEstablishmentAddress: Typeclass[EstablishmentAddress] = gen[EstablishmentAddress]

  def generateAcc14WithAddresses(): DisplayDeclaration = {
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

}
