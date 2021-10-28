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

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxSemigroup
import cats.syntax.eq.catsSyntaxEq
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementClaimAnswer.ReimbursementClaimOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyCodesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, DutyType, DutyTypes, Reimbursement, TaxCode, TaxCodes}

final case class ReimbursementClaimAnswer(reimbursementClaims: Map[DutyType, Map[TaxCode, Reimbursement]]) {

  def total: BigDecimal =
    reimbursementClaims.values.foldLeft(BigDecimal(0))((amount, reimbursement) => amount + reimbursement.subtotal)

  def toClaimsAnswer: Option[ClaimsAnswer] = {

    def toClaim(taxCodeWithClaim: (TaxCode, Reimbursement)) =
      Claim(
        taxCode = taxCodeWithClaim._1,
        paidAmount = taxCodeWithClaim._2.paidAmount,
        claimAmount = taxCodeWithClaim._2.shouldOfPaid
      )

    for {
      combinedReimbursements <- reimbursementClaims.values.reduceOption((x, y) => x |+| y)
      answer                 <- NonEmptyList.fromList(combinedReimbursements.map(toClaim).toList)
    } yield answer
  }
}

object ReimbursementClaimAnswer {

  val none: ReimbursementClaimAnswer = ReimbursementClaimAnswer(Map.empty)

  def initialise(dutyCodesAnswer: DutyCodesAnswer): ReimbursementClaimAnswer = {
    val claimsToAnswer: Map[DutyType, Map[TaxCode, Reimbursement]] = dutyCodesAnswer.dutyCodes.map {
      dutyTypeToTaxCodeTuple =>
        dutyTypeToTaxCodeTuple._1 -> dutyTypeToTaxCodeTuple._2
          .map(taxCodeToReimbursementClaimTuple => taxCodeToReimbursementClaimTuple -> Reimbursement.unclaimed)
          .toMap
    }

    ReimbursementClaimAnswer(claimsToAnswer)
  }

  implicit def dutyPaidAndClaimAmountAnswerFormat: Format[Map[DutyType, Map[TaxCode, Reimbursement]]] =
    new Format[Map[DutyType, Map[TaxCode, Reimbursement]]] {
      override def reads(json: JsValue): JsResult[Map[DutyType, Map[TaxCode, Reimbursement]]] =
        json
          .validate[Map[String, Map[String, Reimbursement]]]
          .map { stringToStringToDutyPaidAndClaimAmounts =>
            stringToStringToDutyPaidAndClaimAmounts.map { dutyTypeTuple =>
              DutyTypes.find(dutyTypeTuple._1) match {
                case Some(dutyType) =>
                  val taxCodeToPaidAndClaimAmounts = dutyTypeTuple._2.map { taxCodeTuple =>
                    TaxCodes.find(taxCodeTuple._1) match {
                      case Some(taxCode) => taxCode -> taxCodeTuple._2
                      case None          => sys.error("Could not convert string to tax code type")
                    }
                  }
                  dutyType -> taxCodeToPaidAndClaimAmounts
                case None           => sys.error("Could not convert string to a duty type")
              }
            }
          }

      override def writes(o: Map[DutyType, Map[TaxCode, Reimbursement]]): JsValue =
        Json.toJson(
          o.map { dutyTypesTuple =>
            (
              dutyTypesTuple._1.repr,
              dutyTypesTuple._2.map { taxCodeTuple =>
                (taxCodeTuple._1.value, taxCodeTuple._2)
              }
            )
          }
        )
    }

  implicit class ReimbursementClaimAnswerOps(private val reimbursementClaimAnswer: ReimbursementClaimAnswer)
      extends AnyVal {

    def isIncompleteReimbursementClaim: Option[(DutyType, TaxCode)] =
      for {
        blankClaimsPerDutyType <- reimbursementClaimAnswer.reimbursementClaims.find(_._2.exists(_._2.isUnclaimed))
        firstClaimPerTaxCode   <- blankClaimsPerDutyType._2.find(_._2.isUnclaimed)
      } yield (blankClaimsPerDutyType._1, firstClaimPerTaxCode._1)

    def updateReimbursementClaim(
      dutyType: DutyType,
      dutyCode: TaxCode,
      reimbursementClaim: Reimbursement
    ): ReimbursementClaimAnswer = {
      val codeToReimbursementClaim: Map[TaxCode, Reimbursement]  =
        reimbursementClaimAnswer.reimbursementClaims(dutyType)
      val updatedReimbursementClaim: Map[TaxCode, Reimbursement] =
        codeToReimbursementClaim + (dutyCode                                            -> reimbursementClaim)
      ReimbursementClaimAnswer(reimbursementClaimAnswer.reimbursementClaims + (dutyType -> updatedReimbursementClaim))
    }

    def updateAnswer(dutyCodesAnswer: DutyCodesAnswer): ReimbursementClaimAnswer = {

      val selectedDutyTypes: Set[DutyType] = dutyCodesAnswer.dutyCodes.keys.toSet

      val dutyTypesToDeleteFromAnswer: Set[DutyType] =
        reimbursementClaimAnswer.reimbursementClaims.keys.toSet.diff(selectedDutyTypes)

      val answerWithDeselectedDutyTypes: Map[DutyType, Map[TaxCode, Reimbursement]] =
        reimbursementClaimAnswer.reimbursementClaims.filterNot(dutyTypeToClaimTuple =>
          dutyTypesToDeleteFromAnswer.contains(dutyTypeToClaimTuple._1)
        )

      val dutyTypesToAddToAnswer: Set[DutyType] = selectedDutyTypes.diff(answerWithDeselectedDutyTypes.keys.toSet)

      val newlySelectedDutyTypesWithSelectedTaxCodes: Map[DutyType, Map[TaxCode, Reimbursement]] =
        answerWithDeselectedDutyTypes ++ dutyTypesToAddToAnswer.map(dutyType =>
          dutyType -> dutyCodesAnswer.dutyCodes(dutyType).map(taxCode => taxCode -> Reimbursement.unclaimed).toMap
        )

      val unchangedDutyTypes: Set[DutyType] =
        selectedDutyTypes.intersect(reimbursementClaimAnswer.reimbursementClaims.keySet)

      val unchangedDutyTypesWithTaxCodesDeselected: Set[DutyType] =
        unchangedDutyTypes.filter { dutyType =>
          val newTaxCodeSelection     = dutyCodesAnswer.dutyCodes(dutyType)
          val currentTaxCodeSelection = reimbursementClaimAnswer.reimbursementClaims(dutyType).keys.toList
          if (newTaxCodeSelection.size < currentTaxCodeSelection.size) true
          else if (
            newTaxCodeSelection.size === currentTaxCodeSelection.size
            && newTaxCodeSelection =!= currentTaxCodeSelection // number of duty codes selected is same but different
          ) true
          else false
        }

      val unchangedDutyTypesWithDeselectedTaxCodes: Map[DutyType, Map[TaxCode, Reimbursement]] =
        unchangedDutyTypesWithTaxCodesDeselected.map { dutyType =>
          val newTaxCodeSelection     = dutyCodesAnswer.dutyCodes(dutyType).toSet
          val currentTaxCodeSelection = newlySelectedDutyTypesWithSelectedTaxCodes(dutyType).keySet

          val unchangedTaxCodes: Set[TaxCode] = currentTaxCodeSelection.diff(newTaxCodeSelection)

          val unchangedTaxCodesWithReimbursementClaim: Set[(TaxCode, Reimbursement)] =
            unchangedTaxCodes
              .map(taxCode =>
                newlySelectedDutyTypesWithSelectedTaxCodes(dutyType).filterNot(taxCodeToReimbursementClaimTuple =>
                  taxCodeToReimbursementClaimTuple._1 === taxCode
                )
              )
              .flatMap(taxCodeToReimbursementClaimMap =>
                taxCodeToReimbursementClaimMap.map(taxCodeToReimbursementClaimTuple =>
                  (taxCodeToReimbursementClaimTuple._1, taxCodeToReimbursementClaimTuple._2)
                )
              )

          dutyType -> unchangedTaxCodesWithReimbursementClaim.toMap
        }.toMap

      val answerWithDeselectedTaxCodes: Map[DutyType, Map[TaxCode, Reimbursement]] =
        newlySelectedDutyTypesWithSelectedTaxCodes ++ unchangedDutyTypesWithDeselectedTaxCodes

      val unchangedDutyTypesWithAddedTaxCodes: Set[DutyType] =
        unchangedDutyTypes.filter { dutyType =>
          val newTaxCodeSelection     = dutyCodesAnswer.dutyCodes(dutyType)
          val currentTaxCodeSelection = reimbursementClaimAnswer.reimbursementClaims(dutyType).keys.toList
          if (newTaxCodeSelection.size > currentTaxCodeSelection.size) true else false
        }

      val unchangedDutyTypesWithNewlyAddedTaxCodes: Map[DutyType, Map[TaxCode, Reimbursement]] =
        unchangedDutyTypesWithAddedTaxCodes.map { dutyType =>
          val newTaxCodeSelection         = dutyCodesAnswer.dutyCodes(dutyType).toSet
          val currentTaxCodeSelection     = answerWithDeselectedTaxCodes(dutyType).keySet
          val taxCodesToAdd: Set[TaxCode] = newTaxCodeSelection.diff(currentTaxCodeSelection)

          val unchangedTaxCodes: Map[TaxCode, Reimbursement] = answerWithDeselectedTaxCodes(dutyType)

          val completeTaxCodes: Map[TaxCode, Reimbursement] = {
            val taxCodeToReimbursementClaimMap: Map[TaxCode, Reimbursement] =
              taxCodesToAdd.map(taxCode => taxCode -> Reimbursement.unclaimed).toMap
            taxCodeToReimbursementClaimMap ++ unchangedTaxCodes
          }

          dutyType -> completeTaxCodes
        }.toMap

      ReimbursementClaimAnswer(
        newlySelectedDutyTypesWithSelectedTaxCodes ++
          unchangedDutyTypesWithDeselectedTaxCodes ++
          unchangedDutyTypesWithNewlyAddedTaxCodes
      )
    }
  }

  implicit class ReimbursementClaimOps(val reimbursement: Map[TaxCode, Reimbursement]) extends AnyVal {

    def subtotal: BigDecimal =
      reimbursement.values.foldLeft(BigDecimal(0))((total, claim) => total + claim.refundTotal)
  }

  implicit val format: OFormat[ReimbursementClaimAnswer] = derived.oformat[ReimbursementClaimAnswer]()
}
