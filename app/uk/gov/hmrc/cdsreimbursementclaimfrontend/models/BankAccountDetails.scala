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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import cats.data.Validated.Valid
import cats.data.Validated.invalidNel
import cats.syntax.all._
import play.api.i18n.Messages
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.IncorrectAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse

final case class BankAccountDetails(
  accountName: AccountName,
  sortCode: SortCode,
  accountNumber: AccountNumber,
  bankAccountHasChanged: Boolean = false,
  existenceVerified: Boolean = true
) {

  def masked(implicit messages: Messages): BankAccountDetails =
    BankAccountDetails(
      accountName,
      SortCode(sortCode.masked),
      AccountNumber(accountNumber.masked),
      bankAccountHasChanged,
      existenceVerified
    )

  def withMaybeAccountName(
    nameMatchesOpt: Option[ReputationResponse],
    accountNameOpt: Option[String]
  ): BankAccountDetails =
    (nameMatchesOpt, accountNameOpt) match {
      case (Some(ReputationResponse.Partial), Some(accountName)) =>
        this.copy(accountName = AccountName(accountName))
      case _                                                     =>
        this
    }

  def withExistenceVerified(existenceVerified: Boolean): BankAccountDetails =
    this.copy(existenceVerified = existenceVerified)

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def computeChanges(previous: Option[BankAccountDetails]): BankAccountDetails =
    previous.fold(this)(that =>
      this.copy(bankAccountHasChanged =
        this.accountName != that.accountName ||
          this.sortCode != that.sortCode ||
          this.accountNumber != that.accountNumber
      )
    )

}

object BankAccountDetails {

  def unapply3(bankAccountDetails: BankAccountDetails): Option[(AccountName, SortCode, AccountNumber)] =
    Some((bankAccountDetails.accountName, bankAccountDetails.sortCode, bankAccountDetails.accountNumber))

  val validator: Validator[Option, BankAccountDetails] = (maybeBankDetails: Option[BankAccountDetails]) =>
    maybeBankDetails
      .toValidNel(MissingAnswerError("Bank account details"))
      .andThen(bankDetails =>
        if (!AccountName.isValid(bankDetails.accountName.value))
          invalidNel(IncorrectAnswerError("Account name", "Invalid"))
        else if (!AccountNumber.isValid(bankDetails.accountNumber.value))
          invalidNel(IncorrectAnswerError("Account number", "Invalid"))
        else if (!SortCode.isValid(bankDetails.sortCode.value))
          invalidNel(IncorrectAnswerError("Sort code", "Invalid"))
        else
          Valid(Some(bankDetails))
      )
  implicit val equality: Eq[BankAccountDetails]        =
    Eq.fromUniversalEquals[BankAccountDetails]

  implicit val format: OFormat[BankAccountDetails] =
    Json.using[Json.WithDefaultValues].format[BankAccountDetails]
}
