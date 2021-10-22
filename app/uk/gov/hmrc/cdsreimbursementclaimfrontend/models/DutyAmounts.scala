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

import cats.implicits.catsSyntaxEq
import cats.kernel.Semigroup
import play.api.libs.json.{Json, OFormat}

final case class DutyAmounts(paidAmount: BigDecimal, shouldOfPaid: BigDecimal) {

  lazy val refundableAmount: BigDecimal = paidAmount - shouldOfPaid

  lazy val undefined: Boolean = paidAmount === 0 && shouldOfPaid === 0

  lazy val areValid: Boolean = paidAmount > shouldOfPaid
}

object DutyAmounts {

  val none: DutyAmounts = DutyAmounts(paidAmount = 0, shouldOfPaid = 0)

  implicit val dutyAmountsSemigroup: Semigroup[DutyAmounts] = (claim1: DutyAmounts, claim2: DutyAmounts) =>
    DutyAmounts(
      paidAmount = claim1.paidAmount + claim2.paidAmount,
      shouldOfPaid = claim1.shouldOfPaid + claim2.shouldOfPaid
    )

  implicit val dutyAmountFormat: OFormat[DutyAmounts] = Json.format[DutyAmounts]
}
