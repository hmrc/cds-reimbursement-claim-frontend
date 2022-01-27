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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode

object BankAccountGen {

  lazy val genAccountName: Gen[AccountName] =
    Gen
      .nonEmptyListOf(Gen.alphaChar)
      .map(_.take(40).mkString)
      .map(AccountName(_))

  lazy val genSortCode: Gen[SortCode] =
    Gen
      .listOfN(6, Gen.numChar)
      .map(_.mkString)
      .map(SortCode(_))

  lazy val genAccountNumber: Gen[AccountNumber] =
    Gen
      .listOfN(8, Gen.numChar)
      .map(_.mkString)
      .map(AccountNumber(_))

  lazy val genBankAccountDetails: Gen[BankAccountDetails] =
    for {
      accountName   <- genAccountName
      sortCode      <- genSortCode
      accountNumber <- genAccountNumber
    } yield BankAccountDetails(accountName, sortCode, accountNumber)

  implicit lazy val arbitrarySortCode: Typeclass[SortCode] =
    Arbitrary(genSortCode)

  implicit lazy val arbitraryAccountNumber: Typeclass[AccountNumber] =
    Arbitrary(genAccountNumber)

  implicit lazy val arbitraryAccountName: Typeclass[AccountName] =
    Arbitrary(genAccountName)

  implicit lazy val arbitraryBankAccountDetailsGen: Typeclass[BankAccountDetails] =
    Arbitrary(genBankAccountDetails)

  implicit lazy val arbitraryBankAccountType: Typeclass[BankAccountType] =
    gen[BankAccountType]
}
