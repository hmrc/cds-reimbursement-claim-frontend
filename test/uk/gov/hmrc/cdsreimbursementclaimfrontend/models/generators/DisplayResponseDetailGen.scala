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

import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._

object DisplayResponseDetailGen {

  implicit val arbitraryConsigneeDetails: Typeclass[ConsigneeDetails]           = gen[ConsigneeDetails]
  implicit val arbitraryDeclarantDetails: Typeclass[DeclarantDetails]           = gen[DeclarantDetails]
  implicit val arbitraryConsigneeBankDetails: Typeclass[ConsigneeBankDetails]   = gen[ConsigneeBankDetails]
  implicit val arbitraryDeclarantBankDetails: Typeclass[DeclarantBankDetails]   = gen[DeclarantBankDetails]
  implicit val arbitraryBankDetails: Typeclass[BankDetails]                     = gen[BankDetails]
  implicit val arbitraryMaskedBankDetails: Typeclass[MaskedBankDetails]         = gen[MaskedBankDetails]
  implicit val arbitraryDisplayResponseDetail: Typeclass[DisplayResponseDetail] = gen[DisplayResponseDetail]
}
