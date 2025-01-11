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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementClaim

/** Common properties of the single journey variant. */
trait SingleVariantProperties extends CommonJourneyProperties {

  override def answers: SingleVariantAnswers
  def isSubsidyOnlyJourney: Boolean
  def filterAvailableDuties(duties: Seq[(TaxCode, Boolean)]): Seq[(TaxCode, Boolean)]

  /** Check if all the selected duties have correct amounts provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.correctedAmounts.exists(rc => rc.nonEmpty && rc.forall(_._2.isDefined))

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean =
    !isSubsidyOnlyJourney

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getAvailableDuties: Seq[(TaxCode, Boolean)] = {

    val duties: Seq[(TaxCode, Boolean)] = getNdrcDetails
      .flatMap { ndrcs =>
        val taxCodes = ndrcs
          .map(ndrc =>
            TaxCodes
              .find(ndrc.taxType)
              .map(taxCode => (taxCode, ndrc.isCmaEligible))
          )
          .collect { case Some(x) => x }
        if taxCodes.isEmpty then None else Some(taxCodes)
      }
      .getOrElse(Seq.empty)

    filterAvailableDuties(duties)
  }

  def getSelectedDuties: Option[Seq[TaxCode]] =
    answers.correctedAmounts.map(_.keys.toSeq)

  def isTaxCodeSelected(taxCode: TaxCode): Boolean =
    answers.correctedAmounts.exists(_.contains(taxCode))

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.correctedAmounts.exists(isAllSelectedDutiesAreCMAEligible)

  def isAllSelectedDutiesAreCMAEligible(amounts: Map[TaxCode, Option[ReimbursementClaim]]): Boolean =
    amounts.keySet
      .map(getNdrcDetailsFor)
      .collect { case Some(d) => d }
      .forall(_.isCmaEligible)

  def getSelectedTaxCodesWithCorrectAmount: Seq[(TaxCode, BigDecimal)] =
    answers.correctedAmounts
      .map(
        _.collect { case (taxCode, Some(correctAmount)) => (taxCode, correctAmount.getAmount) }.toSeq
      )
      .getOrElse(Seq.empty)

  def getAvailableTaxCodesWithPaidAmounts: Seq[(TaxCode, BigDecimal)] =
    getLeadDisplayDeclaration
      .flatMap(_.getNdrcDutiesWithAmount)
      .getOrElse(Seq.empty)

  def getReimbursements: Seq[Reimbursement] = {
    val taxCodesWithPaidAmounts: Map[TaxCode, BigDecimal] =
      getAvailableTaxCodesWithPaidAmounts.toMap

    answers.correctedAmounts
      .map(
        _.toSeq
          .flatMap {
            case (taxCode, Some(reimbursementClaim)) =>
              taxCodesWithPaidAmounts.get(taxCode) match {
                case Some(paidAmount) =>
                  Reimbursement.fromCorrectedAmount(
                    taxCode,
                    reimbursementClaim,
                    getDefaultReimbursementMethod,
                    paidAmount
                  )

                case None => Seq.empty
              }

            case _ => Seq.empty
          }
      )
      .getOrElse(Seq.empty)
  }

  def getDefaultReimbursementMethod: ReimbursementMethod =
    if isSubsidyOnlyJourney then ReimbursementMethod.Subsidy
    else answers.reimbursementMethod.getOrElse(ReimbursementMethod.BankAccountTransfer)

  def getUKDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(TaxCodes.ukTaxCodeSet)

  def getEUDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(TaxCodes.euTaxCodeSet)

  def getExciseDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(TaxCodes.exciseTaxCodeSet)

  private def getReimbursementTotalBy(include: TaxCode => Boolean): Option[BigDecimal] =
    getReimbursements.foldLeft[Option[BigDecimal]](None) { case (a, Reimbursement(taxCode, amount, _, _, _)) =>
      if include(taxCode) then Some(a.getOrElse(BigDecimal("0.00")) + amount)
      else a
    }

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursements.map(_.amount).sum

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsSingleDocumentTypes)

  def hasCmaReimbursementMethod =
    answers.reimbursementMethod.contains(ReimbursementMethod.CurrentMonthAdjustment)

}
