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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement

import cats.implicits.catsSyntaxEq
import cats.kernel.Semigroup
import play.api.libs.json.{Json, OFormat}

final case class ReimbursementClaim(paidAmount: BigDecimal, shouldOfPaid: BigDecimal) {

  val refundTotal: BigDecimal = paidAmount - shouldOfPaid

  def isBlank: Boolean = paidAmount === 0 && shouldOfPaid === 0
}

object ReimbursementClaim {

  val blank: ReimbursementClaim = ReimbursementClaim(paidAmount = 0, shouldOfPaid = 0)

  implicit val reimbursementSemigroup: Semigroup[ReimbursementClaim] = (x: ReimbursementClaim, y: ReimbursementClaim) =>
    ReimbursementClaim(
      paidAmount = x.paidAmount + y.paidAmount,
      shouldOfPaid = x.shouldOfPaid + y.shouldOfPaid
    )

  implicit val format: OFormat[ReimbursementClaim] = Json.format[ReimbursementClaim]
}
