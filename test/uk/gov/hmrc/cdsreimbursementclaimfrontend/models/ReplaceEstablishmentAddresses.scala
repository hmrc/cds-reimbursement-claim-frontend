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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress

trait ReplaceEstablishmentAddresses {
  def replaceEstablishmentAddresses(
    displayDeclaration: DisplayDeclaration,
    address: EstablishmentAddress
  ): DisplayDeclaration = {
    val consignee                    = displayDeclaration.getConsigneeDetails.map(_.copy(establishmentAddress = address))
    val declarant                    = displayDeclaration.getDeclarantDetails.copy(establishmentAddress = address)
    val updatedDisplayResponseDetail = displayDeclaration.displayResponseDetail
      .copy(
        consigneeDetails = consignee,
        declarantDetails = declarant
      )
    displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetail)
  }
}