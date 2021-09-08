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

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.{genContactDetails, genEstablishmentAddress, genNdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AccountDetailsGen.genAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.{genAccountNumber, genSortCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genEori

object DisplayResponseDetailGen {

  def genDeclarantDetails: Gen[DeclarantDetails] =
    for {
      eori                 <- genEori.map(_.value)
      legalName            <- genStringWithMaxSizeOfN(10)
      establishmentAddress <- genEstablishmentAddress
      contactDetails       <- Gen.option(genContactDetails)
    } yield DeclarantDetails(eori, legalName, establishmentAddress, contactDetails)

  def genConsigneeDetails: Gen[ConsigneeDetails] =
    for {
      eori                 <- genEori.map(_.value)
      legalName            <- genStringWithMaxSizeOfN(10)
      establishmentAddress <- genEstablishmentAddress
      contactDetails       <- Gen.option(genContactDetails)
    } yield ConsigneeDetails(eori, legalName, establishmentAddress, contactDetails)

  def genConsigneeBankDetails: Gen[ConsigneeBankDetails] =
    for {
      accountHolderName <- genStringWithMaxSizeOfN(15)
      sortCode          <- genSortCode
      accountNumber     <- genAccountNumber
    } yield ConsigneeBankDetails(
      accountHolderName,
      sortCode.value,
      accountNumber.value
    )

  def genDeclarantBankDetails: Gen[DeclarantBankDetails] =
    for {
      accountHolderName <- genStringWithMaxSizeOfN(15)
      sortCode          <- genSortCode
      accountNumber     <- genAccountNumber
    } yield DeclarantBankDetails(
      accountHolderName,
      sortCode.value,
      accountNumber.value
    )

  def genBankDetails: Gen[BankDetails] = for {
    consigneeBankDetails <- Gen.option(genConsigneeBankDetails)
    declarantBankDetails <- Gen.option(genDeclarantBankDetails)
  } yield BankDetails(
    consigneeBankDetails = consigneeBankDetails,
    declarantBankDetails = declarantBankDetails
  )

  def genMaskedBankDetails: Gen[MaskedBankDetails] = for {
    consigneeBankDetails <- Gen.option(genConsigneeBankDetails)
    declarantBankDetails <- Gen.option(genDeclarantBankDetails)
  } yield MaskedBankDetails(
    consigneeBankDetails = consigneeBankDetails,
    declarantBankDetails = declarantBankDetails
  )

  def genDisplayResponseDetail: Gen[DisplayResponseDetail] =
    for {
      declarationId            <- genStringWithMaxSizeOfN(10)
      acceptanceDate           <- genLocalDateTime
      declarantReferenceNumber <- Gen.option(genStringWithMaxSizeOfN(10))
      securityReason           <- Gen.option(genStringWithMaxSizeOfN(10))
      btaDueDate               <- Gen.option(genLocalDateTime)
      procedureCode            <- genStringWithMaxSizeOfN(5)
      btaSource                <- Gen.option(genStringWithMaxSizeOfN(10))
      declarantDetails         <- genDeclarantDetails
      consigneeDetails         <- Gen.option(genConsigneeDetails)
      accountDetails           <- Gen.option(Gen.nonEmptyListOf(genAccountDetails))
      bankDetails              <- Gen.option(genBankDetails)
      maskedBankDetails        <- Gen.option(genMaskedBankDetails)
      ndrcDetails              <- Gen.option(Gen.nonEmptyListOf(genNdrcDetails))
    } yield DisplayResponseDetail(
      declarationId = declarationId,
      acceptanceDate = acceptanceDate.toString,
      declarantReferenceNumber = declarantReferenceNumber,
      securityReason = securityReason,
      btaDueDate = btaDueDate.map(_.toString),
      procedureCode = procedureCode,
      btaSource = btaSource,
      declarantDetails = declarantDetails,
      consigneeDetails = consigneeDetails,
      accountDetails = accountDetails,
      bankDetails = bankDetails,
      maskedBankDetails = maskedBankDetails,
      ndrcDetails = ndrcDetails
    )

  implicit val arbitraryDeclarantDetails: Typeclass[DeclarantDetails]           = Arbitrary(genDeclarantDetails)
  implicit val arbitraryConsigneeDetails: Typeclass[ConsigneeDetails]           = Arbitrary(genConsigneeDetails)
  implicit val arbitraryConsigneeBankDetails: Typeclass[ConsigneeBankDetails]   = Arbitrary(genConsigneeBankDetails)
  implicit val arbitraryDeclarantBankDetails: Typeclass[DeclarantBankDetails]   = Arbitrary(genDeclarantBankDetails)
  implicit val arbitraryDisplayResponseDetail: Typeclass[DisplayResponseDetail] = Arbitrary(genDisplayResponseDetail)
  implicit val arbitraryBankDetails: Typeclass[BankDetails]                     = Arbitrary(genBankDetails)
  implicit val arbitraryMaskedBankDetails: Typeclass[MaskedBankDetails]         = Arbitrary(genMaskedBankDetails)
}
