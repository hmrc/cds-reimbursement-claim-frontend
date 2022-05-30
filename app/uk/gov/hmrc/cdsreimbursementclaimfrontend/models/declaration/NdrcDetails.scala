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

import cats.syntax.eq._
import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class NdrcDetails(
  taxType: String,
  amount: String,
  paymentMethod: String, // 001 = Immediate Payment, 002 = Duty Deferment, 003 = Cash Account, 004 = Guarantee Account, 005 = Individual Guarantee"
  paymentReference: String,
  cmaEligible: Option[String]
) {

  def isCmaEligible: Boolean = cmaEligible.getOrElse("0") === "1"
}

object NdrcDetails {
  implicit val format: OFormat[NdrcDetails] = Json.format[NdrcDetails]
}
