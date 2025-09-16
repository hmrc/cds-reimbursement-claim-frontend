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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import cats.syntax.eq.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.collection.immutable.SortedMap

/** Common properties of the scheduled journey variant. */
trait ScheduledVariantProperties extends CommonJourneyProperties {

  override def answers: ScheduledVariantAnswers

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.correctedAmounts
      .exists(rc =>
        rc.forall {
          case (DutyType.Excise, claims) =>
            answers.exciseCategories.nonEmpty
            && allSelectedExciseCategoriesHasBeenProvided
            && claims.nonEmpty
            && claims.forall {
              case (taxCode, Some(claimAmounts)) =>
                DutyType.Excise.taxCodes.contains(taxCode)
                && claimAmounts.isValid
              case _                             => false
            }

          case (dutyType, claims) =>
            claims.nonEmpty
            && claims.forall {
              case (taxCode, Some(claimAmounts)) =>
                dutyType.taxCodes.contains(taxCode)
                && claimAmounts.isValid
              case _                             => false
            }
        }
      )

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def getSelectedDocumentType: Option[UploadDocumentType] =
    answers.selectedDocumentType

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getSelectedDutyTypes: Option[Seq[DutyType]] =
    answers.correctedAmounts.map(_.keys.toSeq)

  def getSelectedExciseCategories: Option[Seq[ExciseCategory]] =
    answers.exciseCategories

  def allSelectedExciseCategoriesHasBeenProvided: Boolean =
    getSelectedExciseCategories === computeProvidedExciseCategories

  def computeProvidedExciseCategories: Option[Seq[ExciseCategory]] =
    answers.correctedAmounts
      .flatMap(_.get(DutyType.Excise))
      .map(_.map((tc, _) => ExciseCategory.categoryOf(tc)).toSeq.distinct)

  def getSelectedDuties: SortedMap[DutyType, Seq[TaxCode]] =
    answers.correctedAmounts
      .map(_.view.mapValues(_.keys.toSeq).to(SortedMap))
      .getOrElse(SortedMap.empty)

  def getSelectedDutiesFor(dutyType: DutyType): Option[Seq[TaxCode]] =
    answers.correctedAmounts.flatMap(_.find(_._1 === dutyType).map(_._2.keys.toSeq))

  def getSelectedDutiesFor(exciseCategory: ExciseCategory): Option[Seq[TaxCode]] =
    answers.correctedAmounts.flatMap(
      _.find(_._1 === DutyType.Excise)
        .map(_._2.keys.filter(tc => ExciseCategory.categoryOf(tc) === exciseCategory).toSeq)
    )

  def getFirstDutyToClaim: Option[(DutyType, TaxCode)] =
    getSelectedDuties.headOption
      .flatMap { case (dt, tcs) =>
        tcs.headOption.map(tc => (dt, tc))
      }

  def findNextSelectedDutyAfter(dutyType: DutyType): Option[DutyType] =
    getSelectedDutyTypes.flatMap(nextAfter(dutyType))

  def findNextSelectedExciseCategoryAfter(exciseCategory: ExciseCategory): Option[ExciseCategory] =
    getSelectedExciseCategories.flatMap(nextAfter(exciseCategory))

  def findNextSelectedTaxCodeAfter(dutyType: DutyType, taxCode: TaxCode): Option[(DutyType, TaxCode | ExciseCategory)] =
    if dutyType == DutyType.Excise
    then
      getSelectedDutiesFor(dutyType).flatMap(nextAfter(taxCode)) match {
        case Some(taxCode) => Some((dutyType, taxCode))
        case None          =>
          findNextSelectedExciseCategoryAfter(ExciseCategory.categoryOf(taxCode))
            .flatMap(exciseCategory =>
              getSelectedDutiesFor(exciseCategory) match {
                case Some(taxCodes) if taxCodes.nonEmpty =>
                  taxCodes.headOption
                    .map(tc => (DutyType.Excise, tc))
                case _                                   =>
                  Some((DutyType.Excise, exciseCategory))
              }
            )
      }
    else
      getSelectedDutiesFor(dutyType).flatMap(nextAfter(taxCode)) match {
        case Some(taxCode) => Some((dutyType, taxCode))
        case None          =>
          findNextSelectedDutyAfter(dutyType)
            .flatMap(dt =>
              getSelectedDutiesFor(dt)
                .flatMap(_.headOption)
                .map(tc => (dt, tc))
            )
      }

  def findNextDutyToSelectDuties: Option[DutyType] =
    answers.correctedAmounts.flatMap(_.find(_._2.isEmpty).map(_._1))

  val isDutyTypeSelected: Boolean = answers.correctedAmounts.exists(_.nonEmpty)

  def getReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]] =
    answers.correctedAmounts
      .map(_.view.mapValues(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) }).to(SortedMap))
      .getOrElse(SortedMap.empty)

  def getReimbursements: SortedMap[DutyType, List[ReimbursementWithCorrectAmount]] =
    answers.correctedAmounts match {
      case Some(correctedAmounts) =>
        correctedAmounts.transform((key, value) => toReimbursementWithCorrectAmount(value, key))
      case None                   => SortedMap.empty
    }

  private def toReimbursementWithCorrectAmount(
    claims: SortedMap[TaxCode, Option[AmountPaidWithCorrect]],
    dutyType: DutyType
  ): List[ReimbursementWithCorrectAmount] =
    claims.view
      .map {
        case (taxCode, Some(amount)) =>
          Some(
            ReimbursementWithCorrectAmount(
              taxCode,
              amount.paidAmount - amount.correctAmount,
              amount.paidAmount,
              amount.correctAmount,
              Some(dutyType)
            )
          )
        case _                       => None
      }
      .collect { case Some(x) => x }
      .toList

  def getReimbursementFor(
    dutyType: DutyType,
    taxCode: TaxCode
  ): Option[AmountPaidWithCorrect] =
    getReimbursementClaimsFor(dutyType).flatMap(_.find(_._1 === taxCode)).flatMap(_._2)

  def getReimbursementClaimsFor(dutyType: DutyType): Option[SortedMap[TaxCode, Option[AmountPaidWithCorrect]]] =
    answers.correctedAmounts.flatMap(_.find(_._1 === dutyType)).map(_._2)

  def getUKDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(_ === DutyType.UkDuty)

  def getEUDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(_ === DutyType.EuDuty)

  def getExciseDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(dt => dt =!= DutyType.UkDuty && dt =!= DutyType.EuDuty)

  private def getReimbursementTotalBy(include: DutyType => Boolean): Option[BigDecimal] = {
    val total = getReimbursementClaims.iterator.map { case (dutyType, reimbursements) =>
      if include(dutyType) then reimbursements.map(_._2.claimAmount).sum else ZERO
    }.sum
    if total === ZERO then None else Some(total)
  }

  def getTaxCodesSubtotal(taxCodes: SortedMap[TaxCode, AmountPaidWithCorrect]): BigDecimal =
    taxCodes.values.foldLeft(BigDecimal(0)) { (total, claim) =>
      total + claim.claimAmount
    }

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.iterator.flatMap(_._2.map(_._2.claimAmount)).sum

  def getTotalPaidAmount: BigDecimal =
    getReimbursementClaims.iterator.flatMap(_._2.map(_._2.paidAmount)).sum

  def getNonExciseDutyClaims: Map[DutyType, List[ReimbursementWithCorrectAmount]] =
    getReimbursements.filter { case (dutyType, _) => dutyType != DutyType.Excise }

  def getExciseClaims: List[ReimbursementWithCorrectAmount] =
    getReimbursements.getOrElse(DutyType.Excise, List.empty)

  def getSelectedExciseCategoryClaims: SortedMap[ExciseCategory, List[ReimbursementWithCorrectAmount]] =
    SortedMap.from(
      getExciseClaims
        .flatMap { reimbursement =>
          reimbursement.taxCode.exciseCategory.map(exciseCategory => (exciseCategory, reimbursement))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap
    )

}
