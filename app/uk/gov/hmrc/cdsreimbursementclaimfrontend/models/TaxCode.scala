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

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait TaxCode extends Product with Serializable

object TaxCode {

  case object A00 extends TaxCode
  case object A20 extends TaxCode
  case object A30 extends TaxCode
  case object A35 extends TaxCode
  case object A40 extends TaxCode
  case object A45 extends TaxCode
  case object B00 extends TaxCode
  case object A50 extends TaxCode
  case object A70 extends TaxCode
  case object A80 extends TaxCode
  case object A85 extends TaxCode
  case object A90 extends TaxCode
  case object A95 extends TaxCode
  case object B05 extends TaxCode

  def fromString(taxCode: String): Option[TaxCode] = taxCode match {
    case "A00" => Some(A00)
    case "A20" => Some(A20)
    case "A30" => Some(A30)
    case "A35" => Some(A35)
    case "A40" => Some(A40)
    case "A45" => Some(A45)
    case "B00" => Some(B00)
    case "A50" => Some(A50)
    case "A70" => Some(A70)
    case "A80" => Some(A80)
    case "A85" => Some(A85)
    case "A90" => Some(A90)
    case "A95" => Some(A95)
    case "B05" => Some(B05)
  }

  def listOfTaxCodes: List[TaxCode]   = List(A00, A20, A30, A35, A40, A45, B00, A50, A70, A80, A90, B05)
  def listOfUKTaxCodes: List[TaxCode] = List(A00, A20, A30, A35, A40, A45, B00)
  def listOfEUTaxCodes: List[TaxCode] = List(A50, A70, A80, A90, B05)

  implicit val format: OFormat[TaxCode] = derived.oformat[TaxCode]()
}
