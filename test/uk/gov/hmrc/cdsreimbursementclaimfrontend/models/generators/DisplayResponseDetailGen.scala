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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{BankDetails, ConsigneeBankDetails, ConsigneeDetails, DeclarantBankDetails, DeclarantDetails, DisplayResponseDetail, MaskedBankDetails}

object DisplayResponseDetailGen extends GenUtils {
  implicit val consigneeDetailsGen: Gen[ConsigneeDetails]           = gen[ConsigneeDetails]
  implicit val declarantDetailsGen: Gen[DeclarantDetails]           = gen[DeclarantDetails]
  implicit val consigneeBankDetailsGen: Gen[ConsigneeBankDetails]   = gen[ConsigneeBankDetails]
  implicit val declarantBankDetailsGen: Gen[DeclarantBankDetails]   = gen[DeclarantBankDetails]
  implicit val bankDetailsGen: Gen[BankDetails]                     = gen[BankDetails]
  implicit val maskedBankDetailsGen: Gen[MaskedBankDetails]         = gen[MaskedBankDetails]
  implicit val displayResponseDetailGen: Gen[DisplayResponseDetail] = gen[DisplayResponseDetail]
}
