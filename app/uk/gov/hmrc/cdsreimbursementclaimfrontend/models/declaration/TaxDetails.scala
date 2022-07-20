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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

final case class TaxDetails(
  taxType: String,
  amount: String
) {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def getTaxCode: TaxCode = TaxCodes
    .find(taxType)
    .getOrElse(
      throw new IllegalArgumentException(
        s"Unsupported taxType=$taxType spotted in ACC14 declaration. Either declaration is wrong or service needs to be updated."
      )
    )

  def getAmount: BigDecimal = BigDecimal(amount)
}

object TaxDetails {
  implicit val format: OFormat[TaxDetails] = Json.format[TaxDetails]
}
