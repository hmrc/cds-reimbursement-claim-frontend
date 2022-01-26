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

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement.PaymentMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement.PaymentReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails

import java.util.UUID

final case class ClaimedReimbursement(
  taxCode: TaxCode,
  paidAmount: BigDecimal,
  claimAmount: BigDecimal,
  id: UUID = UUID.randomUUID(),
  paymentMethod: String = PaymentMethod.`001`,
  paymentReference: String = PaymentReference.notApplicable,
  isFilled: Boolean = false
) {

  def correctedAmount: BigDecimal = paidAmount - claimAmount

  def fillWithCorrectedAmount(correctedAmount: BigDecimal): ClaimedReimbursement =
    copy(isFilled = true, claimAmount = paidAmount - correctedAmount)

}

object ClaimedReimbursement {

  def fromNdrc(ndrc: NdrcDetails): Option[ClaimedReimbursement] =
    TaxCodes
      .find(ndrc.taxType)
      .map(taxType =>
        ClaimedReimbursement(
          paymentMethod = ndrc.paymentMethod,
          paymentReference = ndrc.paymentReference,
          taxCode = taxType,
          paidAmount = BigDecimal(ndrc.amount),
          claimAmount = 0
        )
      )

  def fromDuty(duty: Duty): ClaimedReimbursement = ClaimedReimbursement(
    taxCode = duty.taxCode,
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

  implicit val format: OFormat[ClaimedReimbursement] = Json.format[ClaimedReimbursement]
}
