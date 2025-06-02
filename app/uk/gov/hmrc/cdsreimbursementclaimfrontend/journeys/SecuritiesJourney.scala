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
import com.github.arturopala.validator.Validator
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInSingleOrMultipleShipments
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.containsExportedMethodsOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.TaxDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils

import java.time.LocalDateTime
import scala.collection.immutable.SortedMap
import java.time.Instant

/** An encapsulated Securities journey logic. The constructor of this class MUST stay PRIVATE to protected integrity of
  * the journey.
  *
  * The journey uses two nested case classes:
  *
  *   - [[SecuritiesJourney.Answers]] - keeps record of user answers and acquired documents
  *   - [[SecuritiesJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class SecuritiesJourney private (
  val answers: SecuritiesJourney.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[journeys.SecuritiesJourney.Features]
) extends JourneyBase
    with CommonJourneyProperties
    with DirectFluentSyntax[SecuritiesJourney]
    with SeqUtils
    with JourneyAnalytics {

  type Type = SecuritiesJourney

  val self: SecuritiesJourney = this

  val validate: Validator.Validate[SecuritiesJourney] =
    SecuritiesJourney.validator

  private def copy(
    newAnswers: SecuritiesJourney.Answers
  ): SecuritiesJourney =
    new SecuritiesJourney(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  import SecuritiesJourney.Answers
  import SecuritiesJourney.Checks._
  import SecuritiesJourney.CorrectedAmounts

  override def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  override def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def getDisplayDeclarationIfValidSecurityDepositId(securityDepositId: String): Option[DisplayDeclaration] =
    getLeadDisplayDeclaration
      .flatMap(d => d.getSecurityDetailsFor(securityDepositId).map(_ => d))

  def getIndexOf(securityDepositId: String): Int =
    this.getSelectedDepositIds.indexOf(securityDepositId) + 1

  /** Returns all the security IDs available on the ACC14 declaration. */
  def getSecurityDepositIds: Seq[String] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityDepositIds)
      .getOrElse(Seq.empty)

  def getSecurityDetails: Seq[SecurityDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityDetails)
      .getOrElse(Seq.empty)

  /** Returns true if the security ID is available on the ACC14 declaration. */
  def isValidSecurityDepositId(securityDepositId: String): Boolean =
    getLeadDisplayDeclaration
      .exists(_.isValidSecurityDepositId(securityDepositId))

  val isSingleSecurity: Boolean =
    getSecurityDetails.size == 1

  def getSecurityDetailsFor(securityDepositId: String): Option[SecurityDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityDetailsFor(securityDepositId))

  def getSecurityTaxDetailsFor(securityDepositId: String, taxCode: TaxCode): Option[TaxDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityTaxDetailsFor(securityDepositId, taxCode))

  /** For the given deposit ID returns amount atributed to the given tax type (duty). */
  def getSecurityDepositAmountFor(securityDepositId: String, taxCode: TaxCode): Option[BigDecimal] =
    getSecurityTaxDetailsFor(securityDepositId, taxCode).map(_.getAmount)

  /** For the given deposit ID returns total amount. */
  def getTotalSecurityDepositAmountFor(securityDepositId: String): Option[BigDecimal] =
    getSecurityDetailsFor(securityDepositId).map(_.getTotalAmount)

  /** For the given deposit ID returns all declared tax types (duties). */
  def getSecurityTaxCodesFor(securityDepositId: String): Seq[TaxCode] =
    getLeadDisplayDeclaration
      .map(_.getSecurityTaxCodesFor(securityDepositId))
      .getOrElse(Seq.empty)

  def getSecurityTaxCodesWithAmounts(securityDepositId: String): Seq[DutyAmount] =
    getSecurityTaxCodesFor(securityDepositId)
      .flatMap(getSecurityTaxDetailsFor(securityDepositId, _).toList)
      .map(x => DutyAmount(x.getTaxCode, x.getAmount))

  /** Returns deposit IDs selected by the user. */
  def getSelectedDepositIds: Seq[String] =
    answers.correctedAmounts.map(_.keys.toSeq).getOrElse(Seq.empty)

  /** Returns true if deposit ID has been selected by the user. */
  def isSelectedDepositId(securityDepositId: String): Boolean =
    answers.correctedAmounts.exists(_.contains(securityDepositId))

  def getSecuritySelectionStatus(securityDepositId: String): Option[YesNo] =
    if isSelectedDepositId(securityDepositId) then Some(YesNo.Yes)
    else if answers.modes.checkDeclarationDetailsChangeMode || answers.checkYourAnswersChangeMode then Some(YesNo.No)
    else None

  def getSelectedDutiesFor(securityDepositId: String): Option[Seq[TaxCode]] =
    answers.correctedAmounts.flatMap(
      _.get(securityDepositId)
        .flatMap(_.noneIfEmpty)
        .map(_.keys.toSeq)
    )

  def getAllSelectedDuties: Seq[(String, TaxCode)] =
    answers.correctedAmounts
      .map(_.toSeq.flatMap { case (sid, reclaims) =>
        reclaims.keys.map(tc => (sid, tc))
      })
      .getOrElse(Seq.empty)

  def getTotalClaimAmount: BigDecimal =
    answers.correctedAmounts
      .map(_.map { case (sid, correctAmounts) =>
        val depositAmounts = getSecurityDepositAmountsFor(sid)
        correctAmounts.map { case (taxCode, amountOpt) =>
          amountOpt.map(amount => depositAmounts(taxCode) - amount).getOrElse(ZERO)
        }.sum
      }.sum)
      .getOrElse(ZERO)

  def getTotalClaimAmountFor(securityDepositId: String): Option[BigDecimal] = {
    val depositAmounts = getSecurityDepositAmountsFor(securityDepositId)
    answers.correctedAmounts
      .flatMap(_.get(securityDepositId).flatMap(_.noneIfEmpty))
      .map(_.map { case (taxCode, amountOpt) =>
        amountOpt.map(amount => depositAmounts(taxCode) - amount).getOrElse(ZERO)
      }.sum)
  }

  def getClaimAmountFor(securityDepositId: String, taxCode: TaxCode): Option[BigDecimal] =
    answers.correctedAmounts
      .flatMap(_.get(securityDepositId))
      .flatMap(_.get(taxCode))
      .flatten
      .flatMap(amount => getSecurityDepositAmountFor(securityDepositId, taxCode).map(_ - amount))

  def isFullSecurityAmountClaimed(securityDepositId: String): Boolean =
    (getTotalSecurityDepositAmountFor(securityDepositId), getTotalClaimAmountFor(securityDepositId)) match {
      case (Some(declarationAmount), Some(claimAmount)) if declarationAmount === claimAmount => true
      case _                                                                                 => false
    }

  def getClaimFullAmountStatus(securityDepositId: String): Option[YesNo] =
    getTotalClaimAmountFor(securityDepositId)
      .map(claimAmount => getTotalSecurityDepositAmountFor(securityDepositId).contains(claimAmount))
      .map(YesNo.of)

  def getSecurityDepositAmountsFor(securityDepositId: String): Map[TaxCode, BigDecimal] =
    getSecurityDetailsFor(securityDepositId)
      .map(_.taxDetails.map(td => (td.getTaxCode, td.getAmount)).toMap)
      .getOrElse(Map.empty)

  def getSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]] =
    answers.correctedAmounts
      .map(
        _.view
          .map { case (securityDepositId, correctedAmounts) =>
            val depositAmounts = getSecurityDepositAmountsFor(securityDepositId)
            (
              securityDepositId,
              correctedAmounts.collect { case (taxCode, Some(amount)) =>
                (taxCode, depositAmounts(taxCode) - amount)
              }
            )
          }
          .to(SortedMap)
      )
      .getOrElse(SortedMap.empty)

  def getReclaimWithAmounts: SortedMap[String, List[ReclaimWithAmounts]] =
    answers.correctedAmounts match {
      case Some(correctedAmounts) =>
        correctedAmounts.transform((key, value) => toClaimWithCorrectAmount(value, key))
      case None                   => SortedMap.empty
    }

  private def toClaimWithCorrectAmount(
    correctedAmounts: CorrectedAmounts,
    securityDepositId: String
  ): List[ReclaimWithAmounts] =
    correctedAmounts.view.collect { case (taxCode, Some(correctAmount)) =>
      val depositAmountsForSecurityDepositId = getSecurityDepositAmountsFor(securityDepositId)
      val paidAmount                         = depositAmountsForSecurityDepositId(taxCode)
      ReclaimWithAmounts(
        taxCode = taxCode,
        claimAmount = paidAmount - correctAmount,
        paidAmount = paidAmount
      )
    }.toList

  def hasCompleteSecuritiesReclaims: Boolean =
    answers.correctedAmounts.nonEmpty &&
      answers.correctedAmounts.forall(m =>
        m.nonEmpty && m.forall(_._2.nonEmpty) && m.forall(_._2.forall(_._2.isDefined))
      )

  // Returns Left(depositId) if duty selection is missing or Right((depositId, taxCode)) claim is missing
  def getNextDepositIdAndTaxCodeToClaim: Option[Either[String, (String, TaxCode)]] =
    answers.correctedAmounts.flatMap(_.foldLeft[Option[Either[String, (String, TaxCode)]]](None) {
      case (acc, (depositId, reclaims)) =>
        if acc.isDefined then acc
        else if reclaims.isEmpty then Some(Left(depositId))
        else reclaims.find(_._2.isEmpty).map { case (taxCode, _) => Right((depositId, taxCode)) }
    })

  def isAllSelectedDutiesAreGuaranteeEligible: Boolean = {
    val selected = getSelectedDepositIds
    selected.nonEmpty && selected
      .map(getSecurityDetailsFor)
      .collect { case Some(s) => s }
      .forall(_.isGuaranteeEligible)
  }

  def isAllDeclaredDutiesAreGuaranteeEligible: Boolean =
    getLeadDisplayDeclaration.exists(
      _.getAllSecurityMethodsOfPayment
        .exists(mps => mps.nonEmpty && mps.forall(mp => mp == "004" || mp == "005"))
    )

  override def needsBanksAccountDetailsSubmission: Boolean =
    needsPayeeTypeSelection
      && reasonForSecurityIsIPROrENU
      || (getSelectedDepositIds.nonEmpty
        && !isAllSelectedDutiesAreGuaranteeEligible)

  override def needsPayeeTypeSelection: Boolean =
    if reasonForSecurityIsIPROrENU
    then !isAllDeclaredDutiesAreGuaranteeEligible
    else !isAllSelectedDutiesAreGuaranteeEligible

  def submitPayeeType(payeeType: PayeeType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if needsPayeeTypeSelection
      then
        if answers.payeeType.contains(payeeType) then
          Right(copy(newAnswers = answers.copy(payeeType = Some(payeeType))))
        else
          Right(
            copy(newAnswers =
              answers.copy(
                payeeType = Some(payeeType),
                bankAccountDetails = None
              )
            )
          )
      else Left("submitPayeeType.unexpected")
    }

  def needsMethodOfDisposalSubmission: Boolean =
    getReasonForSecurity.exists(ReasonForSecurity.ntas)

  def needsExportMRNSubmission: Boolean =
    (needsMethodOfDisposalSubmission, answers.temporaryAdmissionMethodsOfDisposal) match {
      case (true, Some(methods)) =>
        methods.filter(method => TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal.contains(method)).nonEmpty
      case _                     => false
    }

  def getMethodOfDisposal: Option[List[TemporaryAdmissionMethodOfDisposal]] =
    answers.temporaryAdmissionMethodsOfDisposal

  def needsDocumentTypeSelection: Boolean =
    getReasonForSecurity.exists(
      UploadDocumentType
        .securitiesDocumentTypes(
          _,
          answers.temporaryAdmissionMethodsOfDisposal,
          needsProofOfAuthorityForBankAccountDetailsChange
        )
        .isDefined
    )

  def getReasonForSecurity: Option[ReasonForSecurity] =
    answers.reasonForSecurity

  inline def reasonForSecurityIsIPR: Boolean =
    answers.reasonForSecurity.contains(ReasonForSecurity.InwardProcessingRelief)

  inline def reasonForSecurityIsENU: Boolean =
    answers.reasonForSecurity.contains(ReasonForSecurity.EndUseRelief)

  inline def reasonForSecurityIsIPROrENU: Boolean =
    reasonForSecurityIsIPR || reasonForSecurityIsENU

  def reasonForSecurityIsNidac: Boolean =
    answers.reasonForSecurity.exists(ReasonForSecurity.nidac.contains)

  def requiresBillOfDischargeForm: Boolean =
    reasonForSecurityIsIPROrENU

  def needsReimbursementAmountSubmission: Boolean =
    !reasonForSecurityIsIPROrENU

  def needsOtherSupportingEvidence: Boolean =
    !needsAddOtherDocuments

  def needsAddOtherDocuments =
    reasonForSecurityIsIPROrENU
      || reasonForSecurityIsNidac

  def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    getReasonForSecurity
      .flatMap(rfs =>
        UploadDocumentType.securitiesDocumentTypes(
          rfs,
          answers.temporaryAdmissionMethodsOfDisposal,
          needsProofOfAuthorityForBankAccountDetailsChange
        )
      )

  def getSelectedDocumentTypeOrDefault: Option[UploadDocumentType] =
    getReasonForSecurity.flatMap { rfs =>
      UploadDocumentType.securitiesDocumentTypes(
        rfs,
        answers.temporaryAdmissionMethodsOfDisposal,
        needsDocumentTypeSelection
      ) match {
        case None =>
          Some(UploadDocumentType.SupportingEvidence)

        case Some(documentTypes) =>
          answers.selectedDocumentType match {
            case Some(selectedDocumentType)
                if documentTypes.contains(
                  selectedDocumentType
                ) || selectedDocumentType === UploadDocumentType.ProofOfAuthority =>
              Some(selectedDocumentType)

            case _ => None
          }
      }
    }

  def countOfExportMovementReferenceNumbers: Int =
    answers.exportMovementReferenceNumbers.map(_.size).getOrElse(0)

  def getIndexOfExportMovementReferenceNumber(mrn: MRN): Option[Int] =
    answers.exportMovementReferenceNumbers.flatMap(_.zipWithIndex.find(_._1 === mrn).map(_._2))

  /** Resets the journey with the new MRN or keep an existing journey if submitted the same MRN.
    */
  def submitMovementReferenceNumber(
    mrn: MRN
  ): SecuritiesJourney =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn) if existingMrn === mrn =>
          this

        case _ =>
          this.copy(
            Answers(
              userEoriNumber = answers.userEoriNumber,
              movementReferenceNumber = Some(mrn),
              nonce = answers.nonce
            )
          )
      }
    }

  def submitReasonForSecurityAndDeclaration(
    reasonForSecurity: ReasonForSecurity,
    displayDeclaration: DisplayDeclaration
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMovementReferenceNumber) {
      if !answers.movementReferenceNumber.contains(displayDeclaration.getMRN) then
        Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationMrn")
      else if !displayDeclaration.getReasonForSecurity.contains(reasonForSecurity) then
        Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationRfS")
      else if answers.reasonForSecurity.contains(reasonForSecurity) &&
        answers.displayDeclaration.contains(displayDeclaration)
      then Right(this) // unchanged
      else
        Right(
          this.copy(
            Answers(
              userEoriNumber = answers.userEoriNumber,
              movementReferenceNumber = answers.movementReferenceNumber,
              nonce = answers.nonce,
              reasonForSecurity = Some(reasonForSecurity),
              displayDeclaration = Some(displayDeclaration),
              eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly)
            )
          )
        )
    }

  def submitClaimDuplicateCheckStatus(
    similarClaimExistAlreadyInCDFPay: Boolean
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMovementReferenceNumber & hasReasonForSecurity) {
      Right(
        this.copy(
          answers.copy(
            similarClaimExistAlreadyInCDFPay = Some(similarClaimExistAlreadyInCDFPay)
          )
        )
      )
    }

  def submitTemporaryAdmissionMethodsOfDisposal(
    methodsOfDisposal: List[TemporaryAdmissionMethodOfDisposal]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMRNAndDisplayDeclarationAndRfS & thereIsNoSimilarClaimInCDFPay) {
      if needsMethodOfDisposalSubmission then {
        Right(
          this.copy(
            answers.copy(
              temporaryAdmissionMethodsOfDisposal = Some(methodsOfDisposal),
              exportMovementReferenceNumbers =
                if (
                  methodsOfDisposal
                    .exists(mod => TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal.contains(mod))
                )
                  answers.exportMovementReferenceNumbers
                else None
            )
          )
        )
      } else Left("submitTemporaryAdmissionMethodsOfDisposal.unexpected")
    }

  def submitExportMovementReferenceNumber(
    index: Int,
    exportMrn: MRN
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMRNAndDisplayDeclarationAndRfS & thereIsNoSimilarClaimInCDFPay) {
      if needsExportMRNSubmission then {
        answers.exportMovementReferenceNumbers match {
          case None if index === 0 =>
            Right(this.copy(answers.copy(exportMovementReferenceNumbers = Some(Seq(exportMrn)))))

          case Some(exportMRNs) if index >= 0 && index < exportMRNs.size =>
            val existingMrn = exportMRNs(index)
            if exportMrn === existingMrn then Right(this)
            else {
              if exportMRNs.indexOf(exportMrn) === index then Left("submitExportMovementReferenceNumber.duplicated")
              else
                Right(
                  this.copy(
                    answers.copy(exportMovementReferenceNumbers =
                      Some((exportMRNs.take(index) :+ exportMrn) ++ exportMRNs.drop(index + 1))
                    )
                  )
                )
            }

          case Some(exportMRNs) if index === exportMRNs.size =>
            if exportMRNs.contains(exportMrn) then Left("submitExportMovementReferenceNumber.duplicated")
            else Right(this.copy(answers.copy(exportMovementReferenceNumbers = Some(exportMRNs :+ exportMrn))))

          case _ =>
            Left("submitExportMovementReferenceNumber.indexOutOfBounds")

        }
      } else Left("submitExportMovementReferenceNumber.unexpected")
    }

  def removeExportMovementReferenceNumber(mrn: MRN): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMRNAndDisplayDeclarationAndRfS & thereIsNoSimilarClaimInCDFPay) {
      getIndexOfExportMovementReferenceNumber(mrn) match {
        case None => Left("removeExportMovementReferenceNumber.notFound")

        case Some(index) =>
          Right(
            this.copy(
              answers.copy(
                exportMovementReferenceNumbers = answers.exportMovementReferenceNumbers
                  .map(mrns => mrns.take(index) ++ mrns.drop(index + 1))
              )
            )
          )
      }
    }

  def selectSecurityDepositIds(securityDepositIds: Seq[String]): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if securityDepositIds.isEmpty then Left("selectSecurityDepositIds.emptySelection")
      else if !securityDepositIds.forall(isValidSecurityDepositId) then
        Left("selectSecurityDepositIds.invalidSecurityDepositId")
      else {
        val emptySecuritiesReclaims =
          SortedMap(securityDepositIds.map(sid => (sid, SortedMap.empty[TaxCode, Option[BigDecimal]]))*)
        Right(
          this.copy(
            answers.copy(
              correctedAmounts = answers.correctedAmounts
                .map(m =>
                  (emptySecuritiesReclaims ++ m).view
                    .filterKeys(securityDepositIds.contains(_))
                    .to(SortedMap)
                )
                .orElse(Some(emptySecuritiesReclaims))
            )
          )
        )
      }
    }

  def selectSecurityDepositId(securityDepositId: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if securityDepositId.isEmpty then
        Left(
          "selectSecurityDepositIds.emptySecurityDepositId"
        )
      else if !isValidSecurityDepositId(securityDepositId) then
        Left(
          "selectSecurityDepositIds.invalidSecurityDepositId"
        )
      else {
        if answers.correctedAmounts
            .getOrElse(SortedMap.empty[String, CorrectedAmounts])
            .contains(securityDepositId)
        then Right(this)
        else {
          val emptySecuritiesReclaim =
            SortedMap(securityDepositId -> SortedMap.empty[TaxCode, Option[BigDecimal]])
          Right(
            this.copy(
              answers.copy(
                correctedAmounts = answers.correctedAmounts
                  .map(_ ++ emptySecuritiesReclaim)
                  .orElse(Some(emptySecuritiesReclaim))
              )
            )
          )
        }
      }
    }

  def removeSecurityDepositId(securityDepositId: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if !isValidSecurityDepositId(securityDepositId) then Left("selectSecurityDepositIds.invalidSecurityDepositId")
      else {
        val updatedJourney = this.copy(
          answers.copy(
            correctedAmounts = answers.correctedAmounts
              .map(_ - securityDepositId)
              .noneIfEmpty
          )
        )
        // clear bank account details and type when no longer required after security removal
        Right(
          this.copy(
            updatedJourney.answers.copy(
              bankAccountDetails =
                if updatedJourney.needsBanksAccountDetailsSubmission then updatedJourney.answers.bankAccountDetails
                else None,
              bankAccountType =
                if updatedJourney.needsBanksAccountDetailsSubmission then updatedJourney.answers.bankAccountType
                else None
            )
          )
        )
      }
    }

  def selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(
    securityDepositId: String,
    selectedTaxCodes: Seq[TaxCode]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if !isValidSecurityDepositId(securityDepositId) then
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      else if !isSelectedDepositId(securityDepositId) then
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.securityDepositIdNotSelected")
      else if selectedTaxCodes.isEmpty then
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      else if !getSecurityTaxCodesFor(securityDepositId).containsEachItemOf(selectedTaxCodes) then
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      else {
        if getSelectedDutiesFor(securityDepositId)
            .containsSameElements(selectedTaxCodes)
        then Right(this)
        else {
          val existingReclaims: CorrectedAmounts =
            answers.correctedAmounts
              .flatMap(_.get(securityDepositId))
              .getOrElse(SortedMap.empty)
          val refinedReclaims: CorrectedAmounts  =
            SortedMap(selectedTaxCodes.map(taxCode => taxCode -> existingReclaims.getOrElse(taxCode, None))*)
          Right(
            this.copy(
              answers.copy(
                correctedAmounts = answers.correctedAmounts
                  .map(_ + (securityDepositId -> refinedReclaims))
              )
            )
          )
        }
      }
    }

  def isValidCorrectAmount(correctAmount: BigDecimal, taxDetails: TaxDetails): Boolean =
    correctAmount >= 0 && correctAmount < taxDetails.getAmount

  def isValidClaimAmount(claimAmount: BigDecimal, taxDetails: TaxDetails): Boolean =
    claimAmount > 0 && claimAmount <= taxDetails.getAmount

  def submitCorrectAmount(
    securityDepositId: String,
    taxCode: TaxCode,
    correctAmount: BigDecimal
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if !isValidSecurityDepositId(securityDepositId) then Left("submitCorrectAmount.invalidSecurityDepositId")
      else if !isSelectedDepositId(securityDepositId) then Left("submitCorrectAmount.securityDepositIdNotSelected")
      else if !getSelectedDutiesFor(securityDepositId).exists(_.contains(taxCode)) then
        Left("submitCorrectAmount.invalidTaxCode")
      else if !getSecurityTaxDetailsFor(securityDepositId, taxCode).exists(isValidCorrectAmount(correctAmount, _)) then
        Left("submitCorrectAmount.invalidAmount")
      else {
        val updatedCorrectedAmounts: Option[SortedMap[String, CorrectedAmounts]] =
          answers.correctedAmounts.map(_.map {
            case (sid, reclaims) if sid === securityDepositId =>
              (
                sid,
                reclaims.map {
                  case (tc, _) if tc === taxCode => (tc, Some(correctAmount))
                  case other                     => other
                }
              )

            case other => other
          })

        Right(
          this.copy(
            answers.copy(
              correctedAmounts = updatedCorrectedAmounts
            )
          )
        )
      }
    }

  def submitClaimAmount(
    securityDepositId: String,
    taxCode: TaxCode,
    claimAmount: BigDecimal
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if !isValidSecurityDepositId(securityDepositId) then Left("submitCorrectAmount.invalidSecurityDepositId")
      else if !isSelectedDepositId(securityDepositId) then Left("submitCorrectAmount.securityDepositIdNotSelected")
      else if !getSelectedDutiesFor(securityDepositId).exists(_.contains(taxCode)) then
        Left("submitCorrectAmount.invalidTaxCode")
      else if !getSecurityTaxDetailsFor(securityDepositId, taxCode).exists(isValidClaimAmount(claimAmount, _)) then
        Left("submitCorrectAmount.invalidAmount")
      else {
        val updatedCorrectedAmounts: Option[SortedMap[String, CorrectedAmounts]] =
          answers.correctedAmounts.map(_.map {
            case (sid, reclaims) if sid === securityDepositId =>
              (
                sid,
                reclaims.map {
                  case (tc, _) if tc === taxCode =>
                    (
                      tc,
                      getSecurityTaxDetailsFor(sid, taxCode)
                        .map { taxDetails =>
                          taxDetails.getAmount - claimAmount
                        }
                    )

                  case other => other
                }
              )

            case other => other
          })

        Right(
          this.copy(
            answers.copy(
              correctedAmounts = updatedCorrectedAmounts
            )
          )
        )
      }
    }

  def submitFullCorrectedAmounts(securityDepositId: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if !isValidSecurityDepositId(securityDepositId) then Left("submitFullAmountForReclaim.invalidSecurityDepositId")
      else if !isSelectedDepositId(securityDepositId) then
        Left("submitFullAmountForReclaim.securityDepositIdNotSelected")
      else {
        val updatedCorrectedAmounts: Option[SortedMap[String, CorrectedAmounts]] =
          for
            correctedAmounts <- answers.correctedAmounts
            securityDetails  <- getSecurityDetailsFor(securityDepositId)
          yield correctedAmounts + {
            val fullAmountReclaims: CorrectedAmounts =
              SortedMap(
                securityDetails.taxDetails
                  .map(td => td.getTaxCode -> Some(ZERO))*
              )
            (
              securityDepositId,
              fullAmountReclaims
            )
          }

        Right(
          this.copy(
            answers.copy(
              correctedAmounts = updatedCorrectedAmounts
            )
          )
        )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if needsDeclarantAndConsigneeEoriSubmission then
        if getConsigneeEoriFromACC14 match {
            case Some(eori) => eori === consigneeEoriNumber
            case None       => getDeclarantEoriFromACC14.contains(consigneeEoriNumber)
          }
        then
          Right(
            this.copy(
              answers.copy(eoriNumbersVerification =
                answers.eoriNumbersVerification
                  .orElse(Some(EoriNumbersVerification()))
                  .map(_.copy(consigneeEoriNumber = Some(consigneeEoriNumber)))
              )
            )
          )
        else Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if getDeclarantEoriFromACC14.contains(declarantEoriNumber) then
        Right(
          this.copy(
            answers.copy(eoriNumbersVerification =
              answers.eoriNumbersVerification
                .orElse(Some(EoriNumbersVerification()))
                .map(_.copy(declarantEoriNumber = Some(declarantEoriNumber)))
            )
          )
        )
      else Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if needsBanksAccountDetailsSubmission then
        Right(
          this.copy(
            answers.copy(bankAccountDetails =
              Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
            )
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def removeBankAccountDetails(): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if needsBanksAccountDetailsSubmission then
        Right(
          this.copy(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if getDocumentTypesIfRequired.exists(_.contains(documentType)) then
        Right(
          this.copy(
            answers.copy(selectedDocumentType = Some(documentType))
          )
        )
      else Left("submitDocumentTypeSelection.invalid")
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        if getDocumentTypesIfRequired match {
            case Some(dts) => dts.exists(dt => documentType.contains(dt))
            case None      => documentType.contains(UploadDocumentType.SupportingEvidence)
          }
        then {
          val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
            case uf if uf.documentType.isEmpty => uf.copy(cargo = documentType)
            case uf                            => uf
          }
          Right(
            this.copy(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
          )
        } else Left("receiveUploadedFiles.invalidDocumentType")
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckDeclarationDetailsChangeMode(enabled: Boolean): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(modes = answers.modes.copy(checkDeclarationDetailsChangeMode = enabled)))
    }

  def submitClaimFullAmountMode(enabled: Boolean): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(modes = answers.modes.copy(claimFullAmountMode = enabled)))
    }

  def resetClaimFullAmountMode(): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(modes = answers.modes.copy(claimFullAmountMode = true)))
    }

  def submitCheckClaimDetailsChangeMode(enabled: Boolean): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(modes = answers.modes.copy(checkClaimDetailsChangeMode = enabled)))
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(additionalDetails = if additionalDetails.isBlank then None else Some(additionalDetails))
      )
    }

  def submitAdditionalDetailsPageVisited(visited: Boolean): SecuritiesJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(modes =
          answers.modes.copy(
            additionalDetailsPageVisitedMode = visited
          )
        )
      )
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): SecuritiesJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ =>
            this.copy(
              answers.copy(modes =
                answers.modes.copy(
                  checkYourAnswersChangeMode = enabled,
                  claimFullAmountMode = true
                )
              )
            )
        )
    }

  def receiveBillOfDischargeDocuments(
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        Right(
          this.copy(answers.copy(billOfDischargeDocuments = uploadedFiles))
        )
      } else Left("receiveBillOfDischargeDocuments.invalidNonce")
    }

  def receiveProofOfOriginDocuments(
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        Right(
          this.copy(answers.copy(proofOfOriginDocuments = uploadedFiles))
        )
      } else Left("receiveProofOfOriginDocuments.invalidNonce")
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new SecuritiesJourney(
                answers = this.answers,
                startTimeSeconds = this.startTimeSeconds,
                caseNumber = Some(caseNumber),
                submissionDateTime = Some(LocalDateTime.now()),
                features = this.features
              )
            )
        )
    }

  override def equals(obj: Any): Boolean =
    obj match {
      case that: SecuritiesJourney =>
        that.answers === this.answers && that.caseNumber === this.caseNumber
      case _                       => false
    }

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"SecuritiesJourney${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the journey and retrieves the output. */

  def toOutput: Either[Seq[String], SecuritiesJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for
          mrn                 <- getLeadMovementReferenceNumber
          rfs                 <- getReasonForSecurity
          claimantInformation <- getClaimantInformation
        yield SecuritiesJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = getPayeeTypeForOutput(answers.payeeType),
          displayPayeeType = answers.payeeType,
          claimantInformation = claimantInformation,
          reasonForSecurity = rfs,
          securitiesReclaims = getSecuritiesReclaims,
          bankAccountDetails =
            if needsBanksAccountDetailsSubmission then answers.bankAccountDetails
            else None,
          supportingEvidences = answers.supportingEvidences.map(EvidenceDocument.from)
            ++ answers.billOfDischargeDocuments.map(EvidenceDocument.from)
            ++ answers.proofOfOriginDocuments.map(EvidenceDocument.from),
          temporaryAdmissionMethodsOfDisposal = answers.temporaryAdmissionMethodsOfDisposal match {
            case Some(mods) if mods.contains(ExportedInSingleOrMultipleShipments) =>
              def updateModsWith(
                methodOfDisposal: TemporaryAdmissionMethodOfDisposal
              ): List[TemporaryAdmissionMethodOfDisposal] =
                mods.updated(mods.indexOf(ExportedInSingleOrMultipleShipments), methodOfDisposal)

              answers.exportMovementReferenceNumbers match {
                case Some(exportMRNs) if exportMRNs.size == 1 => Some(updateModsWith(ExportedInSingleShipment))
                case _                                        => Some(updateModsWith(ExportedInMultipleShipments))
              }

            case other => other
          },
          exportMovementReferenceNumber = answers.temporaryAdmissionMethodsOfDisposal match {
            case Some(mods) if containsExportedMethodsOfDisposal(mods) => answers.exportMovementReferenceNumbers
            case _                                                     => None
          },
          additionalDetails = answers.additionalDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object SecuritiesJourney extends JourneyCompanion[SecuritiesJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): SecuritiesJourney =
    new SecuritiesJourney(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = SortedMap[TaxCode, Option[BigDecimal]]

  final case class Features(availableReasonsForSecurity: Set[ReasonForSecurity])

  object Features {
    implicit val format: Format[Features] = Json.format[Features]
  }

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    reasonForSecurity: Option[ReasonForSecurity] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    payeeType: Option[PayeeType] = None,
    similarClaimExistAlreadyInCDFPay: Option[Boolean] = None, // TPI04 check flag
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    exportMovementReferenceNumbers: Option[Seq[MRN]] =
      None, // mandatory for some reasons, see ReasonForSecurity.requiresExportDeclaration,
    temporaryAdmissionMethodsOfDisposal: Option[List[TemporaryAdmissionMethodOfDisposal]] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    correctedAmounts: Option[SortedMap[String, CorrectedAmounts]] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    additionalDetails: Option[String] = None,
    billOfDischargeDocuments: Seq[UploadedFile] = Seq.empty,
    proofOfOriginDocuments: Seq[UploadedFile] = Seq.empty,
    modes: SecuritiesJourneyModes = SecuritiesJourneyModes()
  ) extends CommonAnswers {

    final override def checkYourAnswersChangeMode: Boolean =
      modes.checkYourAnswersChangeMode
  }

  final case class Output(
    movementReferenceNumber: MRN,
    payeeType: Option[PayeeType],
    displayPayeeType: Option[PayeeType],
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    reasonForSecurity: ReasonForSecurity,
    securitiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]],
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument],
    temporaryAdmissionMethodsOfDisposal: Option[List[TemporaryAdmissionMethodOfDisposal]],
    exportMovementReferenceNumber: Option[Seq[MRN]],
    additionalDetails: Option[String] = None
  ) extends WafErrorMitigation[Output] {

    override def excludeFreeTextInputs() =
      additionalDetails
        .map(value =>
          (
            Seq(("additional_details", value)),
            this.copy(additionalDetails = Some("Additional details are attached as a separate text file"))
          )
        )
        .getOrElse((Seq.empty, this))
  }

  import JourneyValidationErrors._
  import com.github.arturopala.validator.Validator._

  object Checks extends CommonJourneyChecks[SecuritiesJourney] {

    val hasReasonForSecurity: Validate[SecuritiesJourney] =
      checkIsTrue(journey => journey.getReasonForSecurity.isDefined, MISSING_REASON_FOR_SECURITY)

    val hasMRNAndDisplayDeclarationAndRfS: Validate[SecuritiesJourney] =
      hasMovementReferenceNumber &
        hasDisplayDeclaration &
        hasReasonForSecurity

    val thereIsNoSimilarClaimInCDFPay: Validate[SecuritiesJourney] =
      checkIsTrue[SecuritiesJourney](
        _.answers.similarClaimExistAlreadyInCDFPay.isDefined,
        MISSING_CLAIM_DUPLICATE_CHECK_STATUS_WITH_TPI04
      ) & checkIsTrue[SecuritiesJourney](
        _.answers.similarClaimExistAlreadyInCDFPay.contains(false),
        SIMILAR_CLAIM_EXISTS_ALREADY_IN_CDFPAY
      )

    val reclaimAmountsHasBeenDeclared: Validate[SecuritiesJourney] =
      checkIsTrue[SecuritiesJourney](_.hasCompleteSecuritiesReclaims, INCOMPLETE_SECURITIES_RECLAIMS) &
        checkIsTrue[SecuritiesJourney](
          _.getTotalClaimAmount > 0,
          TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO
        )

    val userCanProceedWithThisClaim: Validate[SecuritiesJourney] =
      hasMRNAndDisplayDeclarationAndRfS &
        thereIsNoSimilarClaimInCDFPay &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified

    val hasMethodOfDisposalIfNeeded: Validate[SecuritiesJourney] =
      conditionally[SecuritiesJourney](
        _.needsMethodOfDisposalSubmission,
        checkIsDefined(_.answers.temporaryAdmissionMethodsOfDisposal, MISSING_METHOD_OF_DISPOSAL),
        checkIsEmpty(_.answers.temporaryAdmissionMethodsOfDisposal, "unexpected method of disposal, should be empty")
      )

    val hasExportMRNIfNeeded: Validate[SecuritiesJourney] =
      conditionally[SecuritiesJourney](
        _.needsExportMRNSubmission,
        checkIsTrue(
          _.answers.exportMovementReferenceNumbers.exists(_.nonEmpty),
          MISSING_EXPORT_MOVEMENT_REFERENCE_NUMBER
        ),
        checkIsEmpty(_.answers.exportMovementReferenceNumbers, "unexpected export MRN, should be empty")
      ) & whenTrue[SecuritiesJourney](
        _.answers.temporaryAdmissionMethodsOfDisposal
          .fold(false)(mods => mods.contains(ExportedInSingleShipment)),
        checkIsTrue(
          _.answers.exportMovementReferenceNumbers.exists(_.size === 1),
          EXPECTED_SINGLE_EXPORT_MOVEMENT_REFERENCE_NUMBER
        )
      ) & whenTrue[SecuritiesJourney](
        _.answers.temporaryAdmissionMethodsOfDisposal
          .fold(false)(mods => mods.contains(ExportedInMultipleShipments)),
        checkIsTrue(
          _.answers.exportMovementReferenceNumbers.exists(_.size > 1),
          EXPECTED_MULTIPLE_EXPORT_MOVEMENT_REFERENCE_NUMBERS
        )
      )

    val hasBillOfDischargeDocumentsIfNeeded: Validate[SecuritiesJourney] =
      conditionally[SecuritiesJourney](
        _.reasonForSecurityIsIPR,
        checkIsTrue(
          _.answers.billOfDischargeDocuments.nonEmpty,
          MISSING_BILL_OF_DISCHARGE_3_DOCUMENTS
        ),
        conditionally[SecuritiesJourney](
          _.reasonForSecurityIsENU,
          checkIsTrue(
            _.answers.billOfDischargeDocuments.nonEmpty,
            MISSING_BILL_OF_DISCHARGE_4_DOCUMENTS
          ),
          checkIsTrue(
            _.answers.billOfDischargeDocuments.isEmpty,
            "unexpected BOD documents, should be empty"
          )
        )
      )

    val hasProofOfOriginIfNeeded: Validate[SecuritiesJourney] =
      whenTrue[SecuritiesJourney](
        _.reasonForSecurityIsNidac,
        checkIsTrue(
          _.answers.proofOfOriginDocuments.nonEmpty,
          MISSING_PROOF_OF_ORIGIN_DOCUMENTS
        )
      )

    val additionalDetailsPageVisited: Validate[SecuritiesJourney] =
      checkIsTrue(
        _.answers.modes.additionalDetailsPageVisitedMode,
        ADDITIONAL_DETAILS_NOT_YET_VISITED
      )

    val reasonForSecurityIsIPR: Validate[SecuritiesJourney] =
      checkIsTrue(
        _.reasonForSecurityIsIPR,
        INVALID_REASON_FOR_SECURITY
      )

    val reasonForSecurityIsENU: Validate[SecuritiesJourney] =
      checkIsTrue(
        _.reasonForSecurityIsENU,
        INVALID_REASON_FOR_SECURITY
      )

    val reasonForSecurityIsIPROrENU: Validate[SecuritiesJourney] =
      checkIsTrue(
        j =>
          j.reasonForSecurityIsIPR
            || j.reasonForSecurityIsENU,
        INVALID_REASON_FOR_SECURITY
      )

    val reasonForSecurityIsNidac: Validate[SecuritiesJourney] =
      checkIsTrue(
        _.reasonForSecurityIsNidac,
        INVALID_REASON_FOR_SECURITY
      )

    val needsAddOtherDocuments: Validate[SecuritiesJourney] =
      checkIsTrue(_.needsAddOtherDocuments, INVALID_REASON_FOR_SECURITY)
  }

  import Checks._

  override implicit val validator: Validate[SecuritiesJourney] =
    Validator.all(
      hasMRNAndDisplayDeclarationAndRfS,
      thereIsNoSimilarClaimInCDFPay,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      hasMethodOfDisposalIfNeeded,
      hasExportMRNIfNeeded,
      whenTrue[SecuritiesJourney](
        _.needsReimbursementAmountSubmission,
        Validator.all(
          reclaimAmountsHasBeenDeclared,
          paymentMethodHasBeenProvidedIfNeeded
        )
      ),
      contactDetailsHasBeenProvided,
      whenTrue[SecuritiesJourney](
        _.needsOtherSupportingEvidence,
        supportingEvidenceHasBeenProvided
      ),
      payeeTypeIsDefined,
      hasBillOfDischargeDocumentsIfNeeded,
      hasProofOfOriginIfNeeded,
      paymentMethodHasBeenProvidedIfNeeded,
      additionalDetailsPageVisited
    )

  import JourneyFormats._

  object Answers {
    implicit val format: Format[Answers] =
      Json.using[Json.WithDefaultValues].format[Answers]
  }

  object Output {
    implicit val format: Format[Output] = Json.format[Output]
  }

  import play.api.libs.functional.syntax._

  implicit val format: Format[SecuritiesJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new SecuritiesJourney(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(journey =>
        (journey.answers, journey.startTimeSeconds, journey.caseNumber, journey.submissionDateTime, journey.features)
      )
    )

  override def tryBuildFrom(answers: Answers, features: Option[Features] = None): Either[String, SecuritiesJourney] =
    empty(answers.userEoriNumber, answers.nonce, features)
      .mapWhenDefined(answers.movementReferenceNumber)(_.submitMovementReferenceNumber)
      .flatMapWhenDefined(
        answers.reasonForSecurity.zip(answers.displayDeclaration)
      )(j => { case (rfs: ReasonForSecurity, decl: DisplayDeclaration) =>
        j.submitReasonForSecurityAndDeclaration(rfs, decl)
      })
      .flatMapWhenDefined(answers.similarClaimExistAlreadyInCDFPay)(_.submitClaimDuplicateCheckStatus)
      .flatMapWhenDefined(answers.temporaryAdmissionMethodsOfDisposal)(_.submitTemporaryAdmissionMethodsOfDisposal)
      .flatMapEachWhenDefined(answers.exportMovementReferenceNumbers.zipWithIndex)(j => { case (mrn: MRN, index: Int) =>
        j.submitExportMovementReferenceNumber(index, mrn)
      })
      .mapWhenDefined(answers.eoriNumbersVerification.flatMap(_.userXiEori))(_.submitUserXiEori)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber))(_.submitConsigneeEoriNumber)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber))(_.submitDeclarantEoriNumber)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress)
      .flatMapEachWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectSecurityDepositId
      )
      .map(_.submitCheckDeclarationDetailsChangeMode(answers.modes.checkDeclarationDetailsChangeMode))
      .flatMapEachWhenDefined(answers.correctedAmounts)((journey: SecuritiesJourney) => {
        case (depositId: String, reclaims: SortedMap[TaxCode, Option[BigDecimal]]) =>
          journey
            .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, reclaims.keySet.toSeq)
            .flatMapEachWhenMappingDefined(reclaims)((journey: SecuritiesJourney) =>
              (taxCode: TaxCode, amount: BigDecimal) => journey.submitCorrectAmount(depositId, taxCode, amount)
            )
      })
      .map(_.submitClaimFullAmountMode(answers.modes.claimFullAmountMode))
      .map(_.submitCheckClaimDetailsChangeMode(answers.modes.checkClaimDetailsChangeMode))
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMap(j => j.receiveBillOfDischargeDocuments(answers.nonce, answers.billOfDischargeDocuments))
      .flatMap(j => j.receiveProofOfOriginDocuments(answers.nonce, answers.proofOfOriginDocuments))
      .map(_.submitAdditionalDetailsPageVisited(answers.modes.additionalDetailsPageVisitedMode))
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

}
