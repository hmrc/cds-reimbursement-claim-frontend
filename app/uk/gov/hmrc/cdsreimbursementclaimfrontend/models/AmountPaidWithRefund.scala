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

final case class AmountPaidWithRefund(paidAmount: BigDecimal, refundAmount: BigDecimal) {

  def isUnclaimed: Boolean = paidAmount === 0 && refundAmount === 0

  def isValid: Boolean = refundAmount >= 0 && refundAmount <= paidAmount
}

object AmountPaidWithRefund {

  val unclaimed: AmountPaidWithRefund = AmountPaidWithRefund(paidAmount = 0, refundAmount = 0)

  implicit val semigroup: Semigroup[AmountPaidWithRefund] =
    (x: AmountPaidWithRefund, y: AmountPaidWithRefund) =>
      AmountPaidWithRefund(
        paidAmount = x.paidAmount + y.paidAmount,
        refundAmount = x.refundAmount + y.refundAmount
      )

  implicit val format: OFormat[AmountPaidWithRefund] = Json.format[AmountPaidWithRefund]
}
