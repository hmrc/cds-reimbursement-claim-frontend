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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType._

trait HaveInspectionDetails {
  self: Journey with RejectedGoodsJourneyProperties =>

  def submitInspectionDate(inspectionDate: InspectionDate): Journey
  def submitInspectionAddress(inspectionAddress: InspectionAddress): Journey

  final def getPotentialInspectionAddresses: Seq[(InspectionAddressType, String)] =
    Seq(
      getConsigneeContactDetailsFromACC14.flatMap(_.showAddress).map(Importer  -> _),
      getDeclarantContactDetailsFromACC14.flatMap(_.showAddress).map(Declarant -> _)
    ).flatten(Option.option2Iterable)

  final def getInspectionAddressForType(
    addressType: InspectionAddressType
  ): Option[InspectionAddress] =
    addressType match {
      case Importer  => self.getConsigneeContactDetailsFromACC14.map(InspectionAddress.ofType(addressType).mapFrom(_))
      case Declarant => self.getDeclarantContactDetailsFromACC14.map(InspectionAddress.ofType(addressType).mapFrom(_))
      case Other     => None
    }

  final def getInspectionAddressType: Option[InspectionAddressType] =
    answers.inspectionAddress.map(_.addressType)
}
