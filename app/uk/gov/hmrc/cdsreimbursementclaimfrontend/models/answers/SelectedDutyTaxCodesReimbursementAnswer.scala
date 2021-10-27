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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import play.api.libs.json.{Format, JsResult, JsValue}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.{dutyTypesRankMap, taxCodesOrdering}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DutyType, DutyTypes, Reimbursement, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SortedMapFormat

import scala.collection.SortedMap

final case class SelectedDutyTaxCodesReimbursementAnswer(
  value: SortedMap[DutyType, SortedMap[TaxCode, Reimbursement]]
) extends AnyVal {

  def getTaxCodes(dutyType: DutyType): List[TaxCode] =
    value.get(dutyType).toList.flatMap(_.keys.toList)

  def reapply(taxCodes: List[TaxCode])(dutyType: DutyType): SelectedDutyTaxCodesReimbursementAnswer = {
    val currentlySelectedTaxCodes = value(dutyType)

    val updatedTaxCodesSelection = SortedMap(
      taxCodes.map(taxCode => taxCode -> currentlySelectedTaxCodes.getOrElse(taxCode, Reimbursement.blank)): _*
    )

    SelectedDutyTaxCodesReimbursementAnswer(
      value - dutyType + (dutyType -> updatedTaxCodesSelection)
    )
  }

  def findNextSelectedDutyAfter(previous: DutyType): Option[DutyType] =
    DutyTypes.all.drop(dutyTypesRankMap(previous) + 1).find(value.contains)
}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object SelectedDutyTaxCodesReimbursementAnswer {

  // Ordering

  private val dutyTypesRankMap = DutyTypes.all.zipWithIndex.toMap
  private val taxCodesRankMap  = DutyTypes.all.map(_.taxCodes).reduce(_ ++ _).toSet.zipWithIndex.toMap

  implicit val dutyTypesOrdering: Ordering[DutyType] = (a: DutyType, b: DutyType) =>
    dutyTypesRankMap(a) compare dutyTypesRankMap(b)

  implicit val taxCodesOrdering: Ordering[TaxCode] = (a: TaxCode, b: TaxCode) =>
    taxCodesRankMap(a) compare taxCodesRankMap(b)

  // Formats

  implicit val sortedTaxCodeReimbursementMapFormat: Format[SortedMap[TaxCode, Reimbursement]] =
    SortedMapFormat[TaxCode, Reimbursement](TaxCode(_), _.value)

  implicit val sortedDutyTaxCodeReimbursementMapFormat: Format[SortedMap[DutyType, SortedMap[TaxCode, Reimbursement]]] =
    SortedMapFormat[DutyType, SortedMap[TaxCode, Reimbursement]](DutyType(_), _.repr)

  implicit val selectedDutyTaxCodesReimbursementAnswerFormat: Format[SelectedDutyTaxCodesReimbursementAnswer] =
    new Format[SelectedDutyTaxCodesReimbursementAnswer] {

      def reads(json: JsValue): JsResult[SelectedDutyTaxCodesReimbursementAnswer] =
        sortedDutyTaxCodeReimbursementMapFormat
          .reads(json)
          .map(SelectedDutyTaxCodesReimbursementAnswer(_))

      def writes(answer: SelectedDutyTaxCodesReimbursementAnswer): JsValue =
        sortedDutyTaxCodeReimbursementMapFormat.writes(answer.value)
    }

  // Construction

  val none: SelectedDutyTaxCodesReimbursementAnswer =
    SelectedDutyTaxCodesReimbursementAnswer(
      SortedMap.empty[DutyType, SortedMap[TaxCode, Reimbursement]]
    )

  def buildFrom(selectedDuties: List[DutyType]): Builder =
    Builder(selectedDuties)

  final case class Builder(selectedDuties: List[DutyType]) extends AnyVal {

    def synchronizingWith(other: SelectedDutyTaxCodesReimbursementAnswer): SelectedDutyTaxCodesReimbursementAnswer =
      SelectedDutyTaxCodesReimbursementAnswer(
        SortedMap(
          selectedDuties.map(selectedDuty =>
            selectedDuty -> other.value.getOrElse(selectedDuty, SortedMap.empty[TaxCode, Reimbursement])
          ): _*
        )
      )
  }
}
