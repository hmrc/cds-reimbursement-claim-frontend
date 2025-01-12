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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genCountry
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genPostcode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

object Acc14Gen {

  lazy val genNdrcDetails: Gen[NdrcDetails] = for
    taxType          <- Gen.oneOf(TaxCodes.all).map(_.value)
    amount           <- Gen.choose(0L, 10000L).map(_.toString)
    paymentMethod    <- Gen.oneOf("001", "002", "003") // 001 = Immediate Payment, 002 = Duty Deferment, 003 = Cash Account
    paymentReference <- genStringWithMaxSizeOfN(18)
    cmaEligible      <- Gen.oneOf(None, Some("0"), Some("1")) // 0 = CMA Not Eligible, 1 = CMA Eligible
  yield NdrcDetails(taxType, amount, paymentMethod, paymentReference, cmaEligible)

  implicit lazy val arbitraryNdrcDetails: Arbitrary[NdrcDetails] =
    Arbitrary(genNdrcDetails)

  def genListNdrcDetails(min: Int = 2, max: Int = 5): Gen[List[NdrcDetails]] = for
    n <- Gen.choose(min, max)
    m <- Gen.listOfN(n, genNdrcDetails)
  yield m

  lazy val genContactDetails: Gen[ContactDetails] =
    for
      contactName  <- genStringWithMaxSizeOfN(7)
      addressLine1 <- Gen.posNum[Int].flatMap(num => genStringWithMaxSizeOfN(7).map(s => s"$num $s"))
      addressLine2 <- Gen.option(genStringWithMaxSizeOfN(10))
      addressLine3 <- Gen.option(genStringWithMaxSizeOfN(10))
      addressLine4 <- Gen.option(genStringWithMaxSizeOfN(10))
      postalCode   <- genPostcode
      countryCode  <- Gen.option(genCountry.map(_.code))
      telephone    <- Gen.option(genUkPhoneNumber.map(_.value))
      emailAddress <- genEmail.map(_.value)
    yield ContactDetails(
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

  implicit lazy val arbitraryContactDetails: Arbitrary[ContactDetails] =
    Arbitrary(genContactDetails)

  lazy val genEstablishmentAddress: Gen[EstablishmentAddress] =
    for
      num          <- Gen.choose(1, 100)
      street       <- genStringWithMaxSizeOfN(7)
      addressLine2 <- Gen.option(genStringWithMaxSizeOfN(10))
      addressLine3 <- Gen.option(genStringWithMaxSizeOfN(20))
      postalCode   <- Gen.option(genPostcode)
      countryCode  <- genCountry
    yield EstablishmentAddress(
      s"$num $street",
      addressLine2,
      addressLine3,
      postalCode,
      countryCode.code
    )

  implicit lazy val arbitraryEstablishmentAddress: Arbitrary[EstablishmentAddress] =
    Arbitrary(genEstablishmentAddress)

  lazy val genDeclarantDetails: Gen[DeclarantDetails] =
    for
      eori                 <- genEori.map(_.value)
      legalName            <- genStringWithMaxSizeOfN(10)
      establishmentAddress <- genEstablishmentAddress
      contactDetails       <- Gen.option(genContactDetails)
    yield DeclarantDetails(eori, legalName, establishmentAddress, contactDetails)

  lazy val genConsigneeDetails: Gen[ConsigneeDetails] = for
    eori                 <- genEori.map(_.value)
    legalName            <- genStringWithMaxSizeOfN(10)
    establishmentAddress <- genEstablishmentAddress
    contactDetails       <- genContactDetails
  yield ConsigneeDetails(eori, legalName, establishmentAddress, Some(contactDetails))

  lazy val genBankAccountDetails: Gen[BankAccountDetails] =
    for
      accountHolderName <- genStringWithMaxSizeOfN(15)
      sortCode          <- genSortCode
      accountNumber     <- genAccountNumber
    yield BankAccountDetails(
      AccountName(accountHolderName),
      sortCode,
      accountNumber
    )

  lazy val genMaskedBankAccountDetails: Gen[BankAccountDetails] =
    genBankAccountDetails.map(mask)

  lazy val genBankDetails: Gen[BankDetails] =
    for
      consigneeBankDetails <- Gen.option(genBankAccountDetails)
      declarantBankDetails <- Gen.option(genBankAccountDetails)
    yield BankDetails(
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

  lazy val genAccountDetails: Gen[AccountDetails] =
    for
      accountType          <- genStringWithMaxSizeOfN(10)
      accountNumber        <- genStringWithMaxSizeOfN(10)
      eori                 <- genEori.map(_.value)
      legalName            <- genStringWithMaxSizeOfN(15)
      contactDetailsOption <- Gen.option(genContactDetails)
    yield AccountDetails(
      accountType,
      accountNumber,
      eori,
      legalName,
      contactDetailsOption
    )

  lazy val genDisplayResponseDetail: Gen[DisplayResponseDetail] =
    for
      declarationId            <- genMRN
      acceptanceDate           <- genAcceptanceDate
      declarantReferenceNumber <- Gen.option(genStringWithMaxSizeOfN(10))
      btaDueDate               <- Gen.option(genLocalDateTime)
      procedureCode            <- genStringWithMaxSizeOfN(5)
      btaSource                <- Gen.option(genStringWithMaxSizeOfN(10))
      declarantDetails         <- genDeclarantDetails
      consigneeDetails         <- Gen.option(genConsigneeDetails)
      accountDetails           <- Gen.option(Gen.nonEmptyListOf(genAccountDetails))
      bankDetails              <- Gen.option(genBankDetails)
      maskedBankDetails        <- Gen.const(bankDetails.map(mask))
      ndrcDetails              <- Gen.option(Gen.nonEmptyListOf(genNdrcDetails))
    yield DisplayResponseDetail(
      declarationId = declarationId.value,
      acceptanceDate = acceptanceDate,
      declarantReferenceNumber = declarantReferenceNumber,
      securityReason = None,
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

  lazy val genDisplayDeclaration: Gen[DisplayDeclaration] =
    genDisplayResponseDetail.map(DisplayDeclaration(_))

  implicit lazy val arbitraryDeclarantDetails: Arbitrary[DeclarantDetails] =
    Arbitrary(genDeclarantDetails)

  implicit lazy val arbitraryConsigneeDetails: Arbitrary[ConsigneeDetails] =
    Arbitrary(genConsigneeDetails)

  implicit lazy val arbitraryBankDetails: Arbitrary[BankDetails] =
    Arbitrary(genBankDetails)

  lazy val arbitraryDisplayResponseDetail: Arbitrary[DisplayResponseDetail] =
    Arbitrary(genDisplayResponseDetail)

  implicit lazy val arbitraryDisplayDeclaration: Arbitrary[DisplayDeclaration] =
    Arbitrary(genDisplayResponseDetail.map(DisplayDeclaration(_)))

}
