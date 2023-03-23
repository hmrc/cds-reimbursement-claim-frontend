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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.implicits.catsSyntaxApply
import cats.implicits.catsSyntaxSemigroup
import play.api.libs.json.Format
import play.api.libs.json.JsResult
import play.api.libs.json.JsValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.SelectedTaxCodesReimbursementOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.dutyTypesRankMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.taxCodesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SortedMapFormat

import scala.collection.immutable.SortedMap
import scala.collection.immutable.ListMap

final case class SelectedDutyTaxCodesReimbursementAnswer(
  value: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]]
) extends AnyVal {

  /** Gets selected tax codes for duty */
  def getTaxCodes(dutyType: DutyType): List[TaxCode] =
    value.get(dutyType).toList.flatMap(_.keys.toList)

  /** Finds next duty following an order */
  def findNextSelectedDutyAfter(previous: DutyType): Option[DutyType] =
    DutyTypes.all.drop(dutyTypesRankMap(previous) + 1).find(value.contains)

  /** Finds next available unclaimed reimbursement */
  def findUnclaimedReimbursement: Option[(DutyType, TaxCode)] =
    for {
      unclaimedReimbursements <- value.find(_._2.exists(_._2.isUnclaimed))
      firstAvailable          <- unclaimedReimbursements._2.find(_._2.isUnclaimed)
    } yield (unclaimedReimbursements._1, firstAvailable._1)

  /** Updates tax codes selection for concrete duty */
  def reapply(taxCodes: List[TaxCode])(dutyType: DutyType): SelectedDutyTaxCodesReimbursementAnswer = {
    val currentlySelectedTaxCodes = value(dutyType)

    val updatedTaxCodesSelection = SortedMap(
      taxCodes.map(taxCode =>
        taxCode -> currentlySelectedTaxCodes.getOrElse(taxCode, AmountPaidWithCorrect.unclaimed)
      ): _*
    )

    SelectedDutyTaxCodesReimbursementAnswer(SortedMap.from(value - dutyType + (dutyType -> updatedTaxCodesSelection)))
  }

  /** Updates claim for given duty and tax code */
  def update(
    duty: DutyType,
    taxCode: TaxCode,
    claim: AmountPaidWithCorrect
  ): Option[SelectedDutyTaxCodesReimbursementAnswer]       =
    for {
      current <- value.get(duty)
      updated <- current.get(taxCode) *> Some(current - taxCode + (taxCode -> claim))
    } yield SelectedDutyTaxCodesReimbursementAnswer(SortedMap.from(value - duty + (duty -> SortedMap.from(updated))))

  /** Summarising same tax code reimbursements together */
  def combine: Option[Map[TaxCode, AmountPaidWithCorrect]] =
    value.values.map(_.toMap).reduceOption((x, y) => x |+| y)

  /** Calculates claimed amount total for all selected tax codes */
  def total: BigDecimal =
    value.values.foldLeft(BigDecimal(0))((amount, reimbursements) => amount + reimbursements.subtotal)
}

object SelectedDutyTaxCodesReimbursementAnswer {

  // Ordering

  private lazy val dutyTypesRankMap = ListMap(DutyTypes.all.zipWithIndex: _*)
  private lazy val taxCodesRankMap  = ListMap(DutyTypes.all.map(_.taxCodes).fold(Seq.empty)(_ ++ _).zipWithIndex: _*)

  implicit val dutyTypesOrdering: Ordering[DutyType] = (a: DutyType, b: DutyType) =>
    dutyTypesRankMap(a) compare dutyTypesRankMap(b)

  implicit val taxCodesOrdering: Ordering[TaxCode] = (a: TaxCode, b: TaxCode) =>
    taxCodesRankMap(a) compare taxCodesRankMap(b)

  // Formats

  implicit val sortedTaxCodeReimbursementMapFormat: Format[SortedMap[TaxCode, AmountPaidWithCorrect]] =
    SortedMapFormat[TaxCode, AmountPaidWithCorrect](TaxCode(_), _.value)

  implicit val sortedDutyTaxCodeReimbursementMapFormat
    : Format[SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]]] =
    SortedMapFormat[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]](DutyType(_), _.repr)

  implicit val selectedDutyTaxCodesReimbursementAnswerFormat: Format[SelectedDutyTaxCodesReimbursementAnswer] =
    new Format[SelectedDutyTaxCodesReimbursementAnswer] {

      def reads(json: JsValue): JsResult[SelectedDutyTaxCodesReimbursementAnswer] =
        sortedDutyTaxCodeReimbursementMapFormat
          .reads(json)
          .map(SelectedDutyTaxCodesReimbursementAnswer(_))

      def writes(answer: SelectedDutyTaxCodesReimbursementAnswer): JsValue =
        sortedDutyTaxCodeReimbursementMapFormat.writes(answer.value)
    }

  // Extensions

  implicit class SelectedTaxCodesReimbursementOps(private val value: SortedMap[TaxCode, AmountPaidWithCorrect])
      extends AnyVal {

    /** Calculates claimed amount total against given tax code */
    def subtotal: BigDecimal =
      value.values.foldLeft(BigDecimal(0)) { (total, claim) =>
        total + claim.refundAmount
      }
  }

  // Construction

  val none: SelectedDutyTaxCodesReimbursementAnswer =
    SelectedDutyTaxCodesReimbursementAnswer(
      SortedMap.empty[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]]
    )

  def buildFrom(selectedDuties: List[DutyType]): Builder =
    Builder(selectedDuties)

  final case class Builder(selectedDuties: List[DutyType]) extends AnyVal {

    def synchronizingWith(other: SelectedDutyTaxCodesReimbursementAnswer): SelectedDutyTaxCodesReimbursementAnswer =
      SelectedDutyTaxCodesReimbursementAnswer(
        SortedMap(
          selectedDuties.map(selectedDuty =>
            selectedDuty -> other.value.getOrElse(selectedDuty, SortedMap.empty[TaxCode, AmountPaidWithCorrect])
          ): _*
        )
      )
  }

  implicit def builderToAnswer(builder: Builder): SelectedDutyTaxCodesReimbursementAnswer =
    builder synchronizingWith SelectedDutyTaxCodesReimbursementAnswer.none
}
