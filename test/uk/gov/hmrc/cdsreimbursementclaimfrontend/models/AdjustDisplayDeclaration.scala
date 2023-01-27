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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties

trait AdjustDisplayDeclaration {
  def adjustWithDeclarantEori(
    displayDeclaration: DisplayDeclaration,
    journey: CommonJourneyProperties
  ): DisplayDeclaration =
    journey.getDeclarantEoriFromACC14
      .fold(displayDeclaration) { eori =>
        val declarant = displayDeclaration.getDeclarantDetails.copy(declarantEORI = eori.value)
        val drd       = displayDeclaration.displayResponseDetail.copy(declarantDetails = declarant)
        displayDeclaration.copy(displayResponseDetail = drd)
      }
}
