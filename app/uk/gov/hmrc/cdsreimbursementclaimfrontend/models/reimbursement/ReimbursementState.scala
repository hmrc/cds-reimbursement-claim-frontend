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

final case class ReimbursementState(
  dutyTypesAnswer: DutyTypesAnswer,
  dutyCodesAnswer: DutyCodesAnswer
)

object ReimbursementState {

  /*

    what you are really doing here is filtering so use the filter functions on a list to do that with the correct predicate function

    you will always receive the latest duty types answer in the session when calling this function

    based on this latest list of selected duty types what we need to do is compare this list with the list of duty types mapped to tax codes in the session, we can have:

    current session duty types list       |       duty codes answer duty types lists          |       outcome
                                          equal                -                                         no change to duty code answer in the session
               deleted types                            deleted ones exist                               return update duty code answer in the session
               deleted types                            deleted ones do not exist                        no change to duty code answer in the session
               add types                                add ones do not exist                            update duty code codes answer in the session to add new types



   */

  //TODO: another case: they dont change the duty type but they change the tax code selection associated to the a duty type - then what? need to remove that from this Map and also alter the duty paud and claim amount map
  def computeReimbursementState(
    dutyTypesAnswer: DutyTypesAnswer,
    dutyCodesAnswer: DutyCodesAnswer
  ): ReimbursementState = {

    // This removes deleted duty types and their associated duty codes in the Map
    val filteredDutyCodes =
      dutyCodesAnswer.dutyCodes.filter(dutyType => dutyTypesAnswer.dutyTypesSelected.contains(dutyType._1))

    // Now add in the ones which are for new duty types added
    val dutyTypesNotInDutyTypeAnswer =
      dutyTypesAnswer.dutyTypesSelected.filterNot(dutyType => filteredDutyCodes.keys.toList.contains(dutyType))

    val updatedDutyCodesAnswer = DutyCodesAnswer(
      filteredDutyCodes ++ dutyTypesNotInDutyTypeAnswer.map(e => (e, List.empty))
    )

    println(s"\n\n\n\n the state is : ${dutyCodesAnswer.toString} and ${updatedDutyCodesAnswer.toString}")

    ReimbursementState(dutyTypesAnswer, updatedDutyCodesAnswer)

  }

}
