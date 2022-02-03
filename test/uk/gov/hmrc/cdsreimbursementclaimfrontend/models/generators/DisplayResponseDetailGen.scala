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

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.genContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.genEstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.genNdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AccountDetailsGen.genAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.genAccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.genSortCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genEori

object DisplayResponseDetailGen {

  lazy val genDeclarantDetails: Gen[DeclarantDetails] =
    for {
      eori                 <- genEori.map(_.value)
      legalName            <- genStringWithMaxSizeOfN(10)
      establishmentAddress <- genEstablishmentAddress
      contactDetails       <- Gen.option(genContactDetails)
    } yield DeclarantDetails(eori, legalName, establishmentAddress, contactDetails)

  lazy val genConsigneeDetails: Gen[ConsigneeDetails] = for {
    eori                 <- genEori.map(_.value)
    legalName            <- genStringWithMaxSizeOfN(10)
    establishmentAddress <- genEstablishmentAddress
    contactDetails       <- genContactDetails
  } yield ConsigneeDetails(eori, legalName, establishmentAddress, Some(contactDetails))

  lazy val genBankAccountDetails: Gen[BankAccountDetails] =
    for {
      accountHolderName <- genStringWithMaxSizeOfN(15)
      sortCode          <- genSortCode
      accountNumber     <- genAccountNumber
    } yield BankAccountDetails(
      AccountName(accountHolderName),
      sortCode,
      accountNumber
    )

  lazy val genMaskedBankAccountDetails: Gen[BankAccountDetails] =
    genBankAccountDetails.map(mask)

  lazy val genBankDetails: Gen[BankDetails] =
    for {
      consigneeBankDetails <- Gen.option(genBankAccountDetails)
      declarantBankDetails <- Gen.option(genBankAccountDetails)
    } yield BankDetails(
      consigneeBankDetails = consigneeBankDetails,
      declarantBankDetails = declarantBankDetails
    )

  def mask(bankAccountDetails: BankAccountDetails): BankAccountDetails = {
    def hideAllExceptTwoLast(value: String): String =
      s"Ends with ${value.substring(value.length - 2)}"

    bankAccountDetails.copy(
      sortCode = SortCode(hideAllExceptTwoLast(bankAccountDetails.sortCode.value)),
      accountNumber = AccountNumber(hideAllExceptTwoLast(bankAccountDetails.accountNumber.value))
    )
  }

  def mask(bankDetails: BankDetails): BankDetails =
    BankDetails(
      consigneeBankDetails = bankDetails.consigneeBankDetails.map(mask),
      declarantBankDetails = bankDetails.declarantBankDetails.map(mask)
    )

  lazy val genDisplayResponseDetail: Gen[DisplayResponseDetail] =
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
      maskedBankDetails        <- Gen.const(bankDetails.map(mask))
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

  implicit lazy val arbitraryDeclarantDetails: Typeclass[DeclarantDetails] =
    Arbitrary(genDeclarantDetails)

  implicit lazy val arbitraryConsigneeDetails: Typeclass[ConsigneeDetails] =
    Arbitrary(genConsigneeDetails)

  implicit lazy val arbitraryDisplayResponseDetail: Typeclass[DisplayResponseDetail] =
    Arbitrary(genDisplayResponseDetail)

  implicit lazy val arbitraryBankDetails: Typeclass[BankDetails] =
    Arbitrary(genBankDetails)
}
