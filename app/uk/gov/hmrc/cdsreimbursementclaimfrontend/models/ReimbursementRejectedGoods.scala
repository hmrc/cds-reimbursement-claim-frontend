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

import cats.implicits.catsSyntaxEq
import cats.kernel.Semigroup
import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class ReimbursementRejectedGoods(paidAmount: BigDecimal, claimAmount: BigDecimal) {
  lazy val refundTotal: BigDecimal = claimAmount

  lazy val isUnclaimed: Boolean = paidAmount === 0 && claimAmount === 0

  lazy val isValid: Boolean = claimAmount >= 0 && claimAmount < paidAmount
}

object ReimbursementRejectedGoods {

  val unclaimed: ReimbursementRejectedGoods = ReimbursementRejectedGoods(paidAmount = 0, claimAmount = 0)

  implicit val reimbursementSemigroup: Semigroup[ReimbursementRejectedGoods] =
    (x: ReimbursementRejectedGoods, y: ReimbursementRejectedGoods) =>
      ReimbursementRejectedGoods(
        paidAmount = x.paidAmount + y.paidAmount,
        claimAmount = x.claimAmount + y.claimAmount
      )

  implicit val format: OFormat[ReimbursementRejectedGoods] = Json.format[ReimbursementRejectedGoods]
}
