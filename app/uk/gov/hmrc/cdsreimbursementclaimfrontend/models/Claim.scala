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

import cats.data.NonEmptyList
import play.api.libs.json.{Json, OFormat}

final case class Claim(
  paymentMethod: String,
  paymentReference: String,
  taxCategory: TaxCategory,
  taxCode: TaxCode,
  paidAmount: BigDecimal,
  claimAmount: BigDecimal,
  isFilled: Boolean
)

object Claim {
  implicit class ListClaimOps(val claims: NonEmptyList[Claim]) {
    def total: BigDecimal = claims.map(_.claimAmount).toList.sum

    def isUkClaim(claim: Claim): Boolean     = TaxCode.listOfUKTaxCodes.contains(claim.taxCode)
    def isEuClaim(claim: Claim): Boolean     = TaxCode.listOfEUTaxCodes.contains(claim.taxCode)
    def isExciseClaim(claim: Claim): Boolean = TaxCode.listOfUKExciseCodes.contains(claim.taxCode)

    def ukClaims(claims: NonEmptyList[Claim]): List[Claim]     = claims.filter(claim => isUkClaim(claim))
    def euClaims(claims: NonEmptyList[Claim]): List[Claim]     = claims.filter(claim => isEuClaim(claim))
    def exciseClaims(claims: NonEmptyList[Claim]): List[Claim] = claims.filter(claim => isExciseClaim(claim))

    def ukClaimTotal: BigDecimal = claims.filter(claim => isUkClaim(claim)).map(_.claimAmount).sum

    def containsUkClaim(claims: NonEmptyList[Claim]): Boolean     = ukClaims(claims).nonEmpty
    def containsEuClaim(claims: NonEmptyList[Claim]): Boolean     = euClaims(claims).nonEmpty
    def containsExciseClaim(claims: NonEmptyList[Claim]): Boolean = exciseClaims(claims).nonEmpty

  }

  implicit val format: OFormat[Claim] = Json.format[Claim]
}
