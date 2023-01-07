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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AdjustDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

trait AddAcc14 {
  this: AdjustDisplayDeclaration =>

  def addAcc14(
    journey: RejectedGoodsMultipleJourney,
    acc14Declaration: DisplayDeclaration
  ): Either[String, RejectedGoodsMultipleJourney] = {
    val nextIndex           = journey.getMovementReferenceNumbers.map(_.size).getOrElse(0)
    val adjustedDeclaration = adjustWithDeclarantEori(acc14Declaration, journey)
    journey
      .submitMovementReferenceNumberAndDeclaration(nextIndex, adjustedDeclaration.getMRN, adjustedDeclaration)
  }
}
