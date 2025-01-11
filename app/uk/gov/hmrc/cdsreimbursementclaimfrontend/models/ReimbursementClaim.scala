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

import play.api.libs.json.Format
import play.api.libs.json.JsError
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import play.api.libs.json.Reads
import play.api.libs.json.Writes

sealed trait ReimbursementClaim {
  def getAmount: BigDecimal
}

final case class DefaultMethodReimbursementClaim(amount: BigDecimal) extends ReimbursementClaim {
  def getAmount: BigDecimal = amount
}

final case class SubsidyReimbursementClaimPart(amount: BigDecimal)

final case class SplitMethodReimbursementClaim(
  default: DefaultMethodReimbursementClaim,
  subsidy: SubsidyReimbursementClaimPart
) extends ReimbursementClaim {
  def getAmount: BigDecimal = default.amount + subsidy.amount
}

object ReimbursementClaim {

  implicit val defaultMethodReimbursementClaimFormat: OFormat[DefaultMethodReimbursementClaim] =
    Json.format[DefaultMethodReimbursementClaim]

  implicit val subsidyReimbursementClaimPartFormat: OFormat[SubsidyReimbursementClaimPart] =
    Json.format[SubsidyReimbursementClaimPart]

  implicit val splitMethodReimbursementClaimFormat: Format[SplitMethodReimbursementClaim] =
    Json.format[SplitMethodReimbursementClaim]

  implicit val reimbursementClaimFormat: Format[ReimbursementClaim] =
    Format(
      Reads {
        case json @ JsObject(fields) =>
          if fields.contains("parts") then splitMethodReimbursementClaimFormat.reads(json)
          else defaultMethodReimbursementClaimFormat.reads(json)

        case other =>
          JsError(s"Expected JSON object but got $other")
      },
      Writes {
        case r: DefaultMethodReimbursementClaim =>
          defaultMethodReimbursementClaimFormat.writes(r)
        case r: SplitMethodReimbursementClaim   =>
          splitMethodReimbursementClaimFormat.writes(r)
      }
    )

}
