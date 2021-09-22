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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import scala.collection.immutable.SortedMap

final case class ReimbursementState(
  dutyTypesAnswer: DutyTypesAnswer,
  dutyCodesAnswer: DutyCodesAnswer
)

object ReimbursementState {

  def computeReimbursementState(
    dutyTypesAnswer: DutyTypesAnswer,
    dutyCodesAnswer: DutyCodesAnswer
  ): ReimbursementState = {

    val dutyCodesForCurrentlySelectedDutyTypes =
      dutyCodesAnswer.dutyCodes
        .filter(dutyTypesToDutyCodesMap =>
          dutyTypesAnswer.dutyTypesSelected
            .contains(dutyTypesToDutyCodesMap._1)
        )

    val dutyCodesForNewlySelectedDutyTypes =
      dutyTypesAnswer.dutyTypesSelected
        .filterNot(dutyType =>
          dutyCodesForCurrentlySelectedDutyTypes.keys.toList
            .contains(dutyType)
        )

    val updatedDutyCodesAnswer = DutyCodesAnswer(
      dutyCodesForCurrentlySelectedDutyTypes ++ dutyCodesForNewlySelectedDutyTypes.map(dutyType =>
        (dutyType, List.empty)
      )
    )

    val dutyTypeToDutyCodesSortedMap = SortedMap[DutyType, List[TaxCode]](
      updatedDutyCodesAnswer.dutyCodes.toSeq.sortBy(dutyTypeToDutyCodeMap => cmp(dutyTypeToDutyCodeMap)): _*
    )

    ReimbursementState(dutyTypesAnswer, DutyCodesAnswer(dutyTypeToDutyCodesSortedMap))

  }

  def cmp(dutyTypeToRankMap: (DutyType, List[TaxCode])): Int =
    DutyType.dutyTypeToRankMap(dutyTypeToRankMap._1)

}
