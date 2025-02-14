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
import play.api.libs.json.Json

final case class Reimbursement(
  taxCode: TaxCode,
  amount: BigDecimal,
  reimbursementMethod: ReimbursementMethod,
  paidAmount: BigDecimal,
  correctedAmount: Option[BigDecimal] = None
)

object Reimbursement {

  def fromCorrectedAmount(
    taxCode: TaxCode,
    reimbursement: ReimbursementClaim,
    defaultReimbursementMethod: ReimbursementMethod,
    paidAmount: BigDecimal
  ): Seq[Reimbursement] =
    reimbursement match {
      case DefaultMethodReimbursementClaim(correctAmount) =>
        Seq(
          Reimbursement(
            taxCode,
            paidAmount - correctAmount,
            defaultReimbursementMethod,
            paidAmount,
            Some(correctAmount)
          )
        )

      case SplitMethodReimbursementClaim(default, subsidy) =>
        Seq(
          Reimbursement(taxCode, paidAmount - subsidy.amount - default.amount, defaultReimbursementMethod, paidAmount),
          Reimbursement(taxCode, subsidy.amount, ReimbursementMethod.Subsidy, paidAmount)
        )
    }

  implicit val format: Format[Reimbursement] =
    Json.format[Reimbursement]
}
