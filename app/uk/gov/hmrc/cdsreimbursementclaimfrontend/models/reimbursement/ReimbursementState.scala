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
import cats.implicits.catsSyntaxEq
import scala.annotation.tailrec

final case class ReimbursementState(
  dutyTypesAnswer: DutyTypesAnswer,
  dutyCodesAnswer: DutyCodesAnswer,
  dutyPaidAndClaimAmountAnswer: Option[DutyPaidAndClaimAmountAnswer]
)

object ReimbursementState {
  def findDutyTypesToRemove(xs: Set[DutyType], ys: Set[DutyType]): Set[DutyType] = ys.diff(xs)
  def findDutyTypesToAdd(xs: Set[DutyType], ys: Set[DutyType]): Set[DutyType]    = xs.diff(ys)

  def addNewDutyCodes(
    dutyTypes: List[DutyType],
    updatedDutyCodesAnswer: Map[DutyType, List[TaxCode]]
  ): Map[DutyType, List[TaxCode]] = {
    @tailrec
    def loop(xs: List[DutyType], updatedDutyCodesAnswer: Map[DutyType, List[TaxCode]]): Map[DutyType, List[TaxCode]] =
      xs match {
        case Nil          => updatedDutyCodesAnswer
        case ::(head, tl) => loop(tl, updatedDutyCodesAnswer + (head -> List()))
      }
    loop(dutyTypes, updatedDutyCodesAnswer)
  }

  //TODO: another case: they dont change the duty type but they change the tax code selection associated to the a duty type - then what? need to remove that from this Map and also alter the duty paud and claim amount map
  def computeReimbursementState(
    dutyTypesAnswer: DutyTypesAnswer,
    dutyCodesAnswer: DutyCodesAnswer,
    maybeDutyPaidAndClaimAnswer: Option[DutyPaidAndClaimAmountAnswer]
  ): ReimbursementState =
    //TODO: implement this and get this done
    if (dutyTypesAnswer.dutyTypesSelected === dutyCodesAnswer.dutyCodes.keys.toList) { // There are no changes to the selection of the duties - the dutytpes is consitent with the dutycodes
      ReimbursementState(dutyTypesAnswer, dutyCodesAnswer, maybeDutyPaidAndClaimAnswer)
    } else { // there changes but we don't know what they are i.e. deselection, additional selections, different combinations of selections, or different permutation of smae size selections
      // either we need to add or remove duty types to the Map
      // if we check what we need to remove - remove those
      val dutyTypesSelected      = dutyTypesAnswer.dutyTypesSelected.toSet
      val dutyTypesWithDutyCodes = dutyCodesAnswer.dutyCodes.keys.toSet
      val onesToRemove           = findDutyTypesToRemove(dutyTypesSelected, dutyTypesWithDutyCodes)

      val newMap: Map[DutyType, List[TaxCode]] =
        dutyCodesAnswer.dutyCodes.filter(d => onesToRemove.contains(d._1))

      val onesToAdd = findDutyTypesToAdd(newMap.keys.toSet, dutyTypesWithDutyCodes)

      val updatedDutyCodesAnswer = DutyCodesAnswer(addNewDutyCodes(onesToAdd.toList, dutyCodesAnswer.dutyCodes))

      val d: Option[DutyPaidAndClaimAmountAnswer] = maybeDutyPaidAndClaimAnswer
        .map { e =>
          e.dutyPaidAndClaimAmountsEntered.filter(k => onesToRemove.contains(k._1))
        }
        .map { d =>
          DutyPaidAndClaimAmountAnswer(d)
        }

      ReimbursementState(dutyTypesAnswer, updatedDutyCodesAnswer, d)
    }
}
