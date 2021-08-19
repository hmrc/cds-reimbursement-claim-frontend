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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{AccountName, AccountNumber, BankAccountDetails, SortCode}

object BankAccountGen {

  implicit val arbitraryAccountName: Typeclass[AccountName] = Arbitrary(
    Gen
      .nonEmptyListOf(Gen.alphaChar)
      .map(_.take(40).mkString)
      .map(AccountName(_))
  )

  implicit val arbitrarySortCode: Typeclass[SortCode] = Arbitrary(
    Gen
      .listOfN(6, Gen.numChar)
      .map(_.mkString)
      .map(SortCode(_))
  )

  implicit val arbitraryAccountNumber: Typeclass[AccountNumber] = Arbitrary(
    Gen
      .listOfN(8, Gen.numChar)
      .map(_.mkString)
      .map(AccountNumber(_))
  )

  implicit val arbitraryBankAccountDetailsGen: Typeclass[BankAccountDetails] =
    gen[BankAccountDetails]
}
