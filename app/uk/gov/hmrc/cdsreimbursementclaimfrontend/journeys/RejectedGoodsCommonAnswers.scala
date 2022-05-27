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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress

/** Common answers of the rejected-goods single, multiple and scheduled journeys. */
trait RejectedGoodsCommonAnswers extends CommonAnswers {

  def basisOfClaim: Option[BasisOfRejectedGoodsClaim]
  def basisOfClaimSpecialCircumstances: Option[String]
  def methodOfDisposal: Option[MethodOfDisposal]
  def detailsOfRejectedGoods: Option[String]
  def inspectionDate: Option[InspectionDate]
  def inspectionAddress: Option[InspectionAddress]
}
