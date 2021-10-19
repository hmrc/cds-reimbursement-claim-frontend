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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Claim.{PaymentMethod, PaymentReference}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails

import java.util.UUID

final case class Claim(
  taxCode: String,
  paidAmount: BigDecimal,
  claimAmount: BigDecimal,
  id: UUID = UUID.randomUUID(),
  paymentMethod: String = PaymentMethod.`001`,
  paymentReference: String = PaymentReference.notApplicable,
  isFilled: Boolean = false
)

object Claim {

  def fromNdrc(ndrc: NdrcDetails): Claim = Claim(
    paymentMethod = ndrc.paymentMethod,
    paymentReference = ndrc.paymentReference,
    taxCode = ndrc.taxType,
    paidAmount = BigDecimal(ndrc.amount),
    claimAmount = 0
  )

  def fromDuty(duty: Duty): Claim = Claim(
    taxCode = duty.taxCode.value,
    paidAmount = 0,
    claimAmount = 0,
    paymentReference = PaymentReference.unknown
  )

  object PaymentMethod {
    lazy val `001`: String = "001"
  }

  object PaymentReference {
    lazy val notApplicable: String = "n/a"
    lazy val unknown: String       = "unknown"
  }

  implicit class ListClaimOps(val claims: NonEmptyList[Claim]) extends AnyVal {

    def total: BigDecimal = claims.map(_.claimAmount).toList.sum

    def isUkClaim(claim: Claim): Boolean     = TaxCode.listOfUKTaxCodes.map(code => code.value).contains(claim.taxCode)
    def isEuClaim(claim: Claim): Boolean     = TaxCode.listOfEUTaxCodes.map(code => code.value).contains(claim.taxCode)
    def isExciseClaim(claim: Claim): Boolean =
      TaxCode.listOfUKExciseCodes.map(code => code.value).contains(claim.taxCode)

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
