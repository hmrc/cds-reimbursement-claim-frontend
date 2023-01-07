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

import cats.implicits.catsSyntaxEq
import cats.kernel.Semigroup
import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class AmountPaidWithCorrect(paidAmount: BigDecimal, correctAmount: BigDecimal) {

  val refundAmount: BigDecimal = paidAmount - correctAmount

  def isUnclaimed: Boolean = paidAmount === 0 && correctAmount === 0

  def isValid: Boolean = correctAmount >= 0 && correctAmount < paidAmount
}

object AmountPaidWithCorrect {

  val unclaimed: AmountPaidWithCorrect = AmountPaidWithCorrect(paidAmount = 0, correctAmount = 0)

  implicit val semigroup: Semigroup[AmountPaidWithCorrect] =
    (x: AmountPaidWithCorrect, y: AmountPaidWithCorrect) =>
      AmountPaidWithCorrect(
        paidAmount = x.paidAmount + y.paidAmount,
        correctAmount = x.correctAmount + y.correctAmount
      )

  implicit val format: OFormat[AmountPaidWithCorrect] = Json.format[AmountPaidWithCorrect]
}
