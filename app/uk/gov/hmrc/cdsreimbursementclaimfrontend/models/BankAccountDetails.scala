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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.libs.json.{Json, OFormat}

final case class BankAccountDetails(
  accountName: AccountName,
  sortCode: SortCode,
  accountNumber: AccountNumber
)

object BankAccountDetails {
  implicit val format: OFormat[BankAccountDetails] = Json.format[BankAccountDetails]
}

final case class AccountNumber(value: String) extends AnyVal

final case class AccountName(value: String) extends AnyVal

object AccountNumber {
  implicit val format: OFormat[AccountNumber] = Json.format[AccountNumber]
}

object AccountName {
  implicit val format: OFormat[AccountName] = Json.format[AccountName]
}
