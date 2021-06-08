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
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.{AccountName, AccountNumber, BankAccountDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode

object BankAccountGen extends GenUtils {

  implicit val accountNameGen: Gen[AccountName] =
    Gen
      .nonEmptyListOf(Gen.alphaChar)
      .map(_.take(40).mkString)
      .map(AccountName(_))

  implicit val accountNameArb: Arbitrary[AccountName] =
    Arbitrary(accountNameGen)

  implicit val sortCodeGen: Gen[SortCode] =
    Gen
      .listOfN(6, Gen.numChar)
      .map(_.mkString)
      .map(SortCode(_))

  implicit val sortCodeArb: Arbitrary[SortCode] =
    Arbitrary(sortCodeGen)

  implicit val accountNumberGen: Gen[AccountNumber] =
    Gen
      .listOfN(8, Gen.numChar)
      .map(_.mkString)
      .map(AccountNumber(_))

  implicit val accountNumberArb: Arbitrary[AccountNumber] =
    Arbitrary(accountNumberGen)

  implicit val bankAccountDetailsGen: Gen[BankAccountDetails] =
    gen[BankAccountDetails]

  def genPersonalBankAccountDetails: Gen[BankAccountDetails] =
    for {
      name          <- accountNameGen
      sortCode      <- sortCodeGen
      accountNumber <- accountNumberGen
    } yield BankAccountDetails(
      accountName = name,
      isBusinessAccount = None,
      sortCode = sortCode,
      accountNumber = accountNumber
    )

  def genBusinessBankAccountDetails: Gen[BankAccountDetails] =
    for {
      name          <- accountNameGen
      sortCode      <- sortCodeGen
      accountNumber <- accountNumberGen
    } yield BankAccountDetails(
      accountName = name,
      isBusinessAccount = Some(true),
      sortCode = sortCode,
      accountNumber = accountNumber
    )
}
