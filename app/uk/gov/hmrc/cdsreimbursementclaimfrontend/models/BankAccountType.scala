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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed abstract class BankAccountType(val value: Int) extends Product with Serializable

object BankAccountType {

  case object BusinessBankAccount extends BankAccountType(0)
  case object PersonalBankAccount extends BankAccountType(1)

  val allAccountTypes: List[BankAccountType]          = List(BusinessBankAccount, PersonalBankAccount)
  val allAccountsIntToType: Map[Int, BankAccountType] = allAccountTypes.map(a => a.value -> a).toMap
  val allAccountsTypeToInt: Map[BankAccountType, Int] = allAccountTypes.map(a => a -> a.value).toMap

  implicit val format: OFormat[BankAccountType] = derived.oformat[BankAccountType]()

  implicit val eq: Eq[BankAccountType] = Eq.fromUniversalEquals[BankAccountType]
}
