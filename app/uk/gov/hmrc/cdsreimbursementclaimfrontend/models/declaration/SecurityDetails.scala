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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import cats.syntax.eq._

final case class SecurityDetails(
  securityDepositId: String,
  private val totalAmount: String,
  private val amountPaid: String,
  paymentMethod: String, // 001 = Immediate Payment, 002 = Duty Deferment, 003 = Cash Account, 004 = Guarantee Account, 005 = Individual Guarantee"
  paymentReference: String,
  taxDetails: List[TaxDetails]
) {

  def isBankAccountPayment: Boolean = paymentMethod === "001"
  def isGuaranteeEligible: Boolean  = !isBankAccountPayment

  def getTotalAmount: BigDecimal = taxDetails.map(_.getAmount).sum

  def getPaidAmount: BigDecimal = BigDecimal(amountPaid)
}

object SecurityDetails {
  implicit val format: OFormat[SecurityDetails] = Json.format[SecurityDetails]
}
