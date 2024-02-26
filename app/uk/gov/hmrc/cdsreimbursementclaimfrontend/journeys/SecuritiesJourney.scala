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

import cats.syntax.eq._
import com.github.arturopala.validator.Validator
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.TaxDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils

import scala.collection.immutable.SortedMap

/** An encapsulated Securities journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[SecuritiesJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[SecuritiesJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class SecuritiesJourney private (
  val answers: SecuritiesJourney.Answers,
  val caseNumber: Option[String] = None
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
    new SecuritiesJourney(newAnswers, caseNumber)

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

  /** Returns true if the security ID is available on the ACC14 declaration. */
  def isValidSecurityDepositId(securityDepositId: String): Boolean =
    getLeadDisplayDeclaration
      .exists(_.isValidSecurityDepositId(securityDepositId))

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

  def withEnterContactDetailsMode(enabled: Boolean): SecuritiesJourney =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

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
    if (isSelectedDepositId(securityDepositId)) Some(YesNo.Yes)
    else if (answers.modes.checkDeclarationDetailsChangeMode || answers.checkYourAnswersChangeMode) Some(YesNo.No)
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

  def getTotalReclaimAmount: BigDecimal =
    answers.correctedAmounts
      .map(_.map { case (sid, correctAmounts) =>
        val depositAmounts = getSecurityDepositAmountsFor(sid)
        correctAmounts.map { case (taxCode, amountOpt) =>
          amountOpt.map(amount => depositAmounts(taxCode) - amount).getOrElse(ZERO)
        }.sum
      }.sum)
      .getOrElse(ZERO)

  def getTotalReclaimAmountFor(securityDepositId: String): Option[BigDecimal] = {
    val depositAmounts = getSecurityDepositAmountsFor(securityDepositId)
    answers.correctedAmounts
      .flatMap(_.get(securityDepositId).flatMap(_.noneIfEmpty))
      .map(_.map { case (taxCode, amountOpt) =>
        amountOpt.map(amount => depositAmounts(taxCode) - amount).getOrElse(ZERO)
      }.sum)
  }

  def getReclaimAmountFor(securityDepositId: String, taxCode: TaxCode): Option[BigDecimal] =
    answers.correctedAmounts
      .flatMap(_.get(securityDepositId))
      .flatMap(_.get(taxCode))
      .flatten
      .flatMap(amount => getSecurityDepositAmountFor(securityDepositId, taxCode).map(_ - amount))

  def isFullSecurityAmountClaimed(securityDepositId: String): Boolean =
    (getTotalSecurityDepositAmountFor(securityDepositId), getTotalReclaimAmountFor(securityDepositId)) match {
      case (Some(declarationAmount), Some(claimAmount)) if declarationAmount === claimAmount => true
      case _                                                                                 => false
    }

  def getClaimFullAmountStatus(securityDepositId: String): Option[YesNo] =
    getTotalReclaimAmountFor(securityDepositId)
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

  def hasCompleteSecuritiesReclaims: Boolean =
    answers.correctedAmounts.nonEmpty &&
      answers.correctedAmounts.forall(m =>
        m.nonEmpty && m.forall(_._2.nonEmpty) && m.forall(_._2.forall(_._2.isDefined))
      )

  // Returns Left(depositId) if duty selection is missing or Right((depositId, taxCode)) claim is missing
  def getNextDepositIdAndTaxCodeToClaim: Option[Either[String, (String, TaxCode)]] =
    answers.correctedAmounts.flatMap(_.foldLeft[Option[Either[String, (String, TaxCode)]]](None) {
      case (acc, (depositId, reclaims)) =>
        if (acc.isDefined) acc
        else if (reclaims.isEmpty) Some(Left(depositId))
        else reclaims.find(_._2.isEmpty).map { case (taxCode, _) => Right((depositId, taxCode)) }
    })

  def isAllSelectedDutiesAreGuaranteeEligible: Boolean = {
    val selected = getSelectedDepositIds
    selected.nonEmpty && selected
      .map(getSecurityDetailsFor)
      .collect { case Some(s) => s }
      .forall(_.isGuaranteeEligible)
  }

  def needsBanksAccountDetailsSubmission: Boolean =
    getSelectedDepositIds.nonEmpty &&
      !isAllSelectedDutiesAreGuaranteeEligible

  def submitPayeeType(payeeType: PayeeType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (answers.payeeType.contains(payeeType))
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
    }

  def needsMethodOfDisposalSubmission: Boolean =
    getReasonForSecurity.exists(ReasonForSecurity.temporaryAdmissions)

  def needsExportMRNSubmission: Boolean =
    needsMethodOfDisposalSubmission &&
      answers.temporaryAdmissionMethodOfDisposal.exists(
        TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal.contains
      )

  def getMethodOfDisposal: Option[TemporaryAdmissionMethodOfDisposal] = answers.temporaryAdmissionMethodOfDisposal

  def needsDocumentTypeSelection: Boolean =
    getReasonForSecurity.exists(
      UploadDocumentType
        .securitiesDocumentTypes(
          _,
          answers.temporaryAdmissionMethodOfDisposal,
          needsProofOfAuthorityForBankAccountDetailsChange
        )
        .isDefined
    )

  def getReasonForSecurity: Option[ReasonForSecurity] =
    answers.reasonForSecurity

  def reasonForSecurityIsIPR: Boolean =
    answers.reasonForSecurity.contains(ReasonForSecurity.InwardProcessingRelief)

  def reasonForSecurityIsEndUseRelief: Boolean =
    answers.reasonForSecurity.contains(ReasonForSecurity.EndUseRelief)

  def requiresBillOfDischargeForm: Boolean =
    reasonForSecurityIsIPR || reasonForSecurityIsEndUseRelief

  def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    getReasonForSecurity
      .flatMap(rfs =>
        UploadDocumentType.securitiesDocumentTypes(
          rfs,
          answers.temporaryAdmissionMethodOfDisposal,
          needsProofOfAuthorityForBankAccountDetailsChange
        )
      )

  def getSelectedDocumentTypeOrDefault: Option[UploadDocumentType] =
    getReasonForSecurity.flatMap { rfs =>
      UploadDocumentType.securitiesDocumentTypes(
        rfs,
        answers.temporaryAdmissionMethodOfDisposal,
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

  /** Resets the journey with the new MRN
    * or keep an existing journey if submitted the same MRN.
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
      if (!answers.movementReferenceNumber.contains(displayDeclaration.getMRN))
        Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationMrn")
      else if (!displayDeclaration.getReasonForSecurity.contains(reasonForSecurity))
        Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationRfS")
      else if (
        answers.reasonForSecurity.contains(reasonForSecurity) &&
        answers.displayDeclaration.contains(displayDeclaration)
      ) Right(this) // unchanged
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

  def submitTemporaryAdmissionMethodOfDisposal(
    methodOfDisposal: TemporaryAdmissionMethodOfDisposal
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMRNAndDisplayDeclarationAndRfS & thereIsNoSimilarClaimInCDFPay) {
      if (needsMethodOfDisposalSubmission) {
        Right(
          this.copy(
            answers.copy(
              temporaryAdmissionMethodOfDisposal = Some(methodOfDisposal)
            )
          )
        )
      } else
        Left("submitTemporaryAdmissionMethodOfDisposal.unexpected")
    }

  def submitExportMovementReferenceNumber(
    exportMrn: MRN
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMRNAndDisplayDeclarationAndRfS & thereIsNoSimilarClaimInCDFPay) {
      if (needsExportMRNSubmission)
        if (answers.movementReferenceNumber.contains(exportMrn))
          Left("submitExportMovementReferenceNumber.duplicated")
        else
          Right(
            this.copy(
              answers.copy(
                exportMovementReferenceNumber = Some(exportMrn)
              )
            )
          )
      else
        Left("submitExportMovementReferenceNumber.unexpected")
    }

  def selectSecurityDepositIds(securityDepositIds: Seq[String]): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (securityDepositIds.isEmpty)
        Left("selectSecurityDepositIds.emptySelection")
      else if (!securityDepositIds.forall(isValidSecurityDepositId))
        Left("selectSecurityDepositIds.invalidSecurityDepositId")
      else {
        val emptySecuritiesReclaims =
          SortedMap(securityDepositIds.map(sid => (sid, SortedMap.empty[TaxCode, Option[BigDecimal]])): _*)
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
      if (securityDepositId.isEmpty)
        Left(
          "selectSecurityDepositIds.emptySecurityDepositId"
        )
      else if (!isValidSecurityDepositId(securityDepositId))
        Left(
          "selectSecurityDepositIds.invalidSecurityDepositId"
        )
      else {
        if (
          answers.correctedAmounts
            .getOrElse(SortedMap.empty[String, CorrectedAmounts])
            .contains(securityDepositId)
        )
          Right(this)
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
      if (!isValidSecurityDepositId(securityDepositId))
        Left("selectSecurityDepositIds.invalidSecurityDepositId")
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
                if (updatedJourney.needsBanksAccountDetailsSubmission) updatedJourney.answers.bankAccountDetails
                else None,
              bankAccountType =
                if (updatedJourney.needsBanksAccountDetailsSubmission) updatedJourney.answers.bankAccountType
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
      if (!isValidSecurityDepositId(securityDepositId))
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      else if (!isSelectedDepositId(securityDepositId))
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.securityDepositIdNotSelected")
      else if (selectedTaxCodes.isEmpty)
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      else if (!getSecurityTaxCodesFor(securityDepositId).containsEachItemOf(selectedTaxCodes))
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      else {
        if (
          getSelectedDutiesFor(securityDepositId)
            .containsSameElements(selectedTaxCodes)
        )
          Right(this)
        else {
          val existingReclaims: CorrectedAmounts =
            answers.correctedAmounts
              .flatMap(_.get(securityDepositId))
              .getOrElse(SortedMap.empty)
          val refinedReclaims: CorrectedAmounts  =
            SortedMap(selectedTaxCodes.map(taxCode => taxCode -> existingReclaims.getOrElse(taxCode, None)): _*)
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

  def submitCorrectAmount(
    securityDepositId: String,
    taxCode: TaxCode,
    correctAmount: BigDecimal
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (!isValidSecurityDepositId(securityDepositId))
        Left("submitCorrectAmount.invalidSecurityDepositId")
      else if (!isSelectedDepositId(securityDepositId))
        Left("submitCorrectAmount.securityDepositIdNotSelected")
      else if (!getSelectedDutiesFor(securityDepositId).exists(_.contains(taxCode)))
        Left("submitCorrectAmount.invalidTaxCode")
      else if (!getSecurityTaxDetailsFor(securityDepositId, taxCode).exists(isValidCorrectAmount(correctAmount, _)))
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

  def submitFullCorrectedAmounts(securityDepositId: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (!isValidSecurityDepositId(securityDepositId))
        Left("submitFullAmountForReclaim.invalidSecurityDepositId")
      else if (!isSelectedDepositId(securityDepositId))
        Left("submitFullAmountForReclaim.securityDepositIdNotSelected")
      else {
        val updatedCorrectedAmounts: Option[SortedMap[String, CorrectedAmounts]] =
          for {
            correctedAmounts <- answers.correctedAmounts
            securityDetails  <- getSecurityDetailsFor(securityDepositId)
          } yield correctedAmounts + {
            val fullAmountReclaims: CorrectedAmounts =
              SortedMap(
                securityDetails.taxDetails
                  .map(td => td.getTaxCode -> Some(ZERO)): _*
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
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (
          getConsigneeEoriFromACC14 match {
            case Some(eori) => eori === consigneeEoriNumber
            case None       => getDeclarantEoriFromACC14.contains(consigneeEoriNumber)
          }
        )
          Right(
            this.copy(
              answers.copy(eoriNumbersVerification =
                answers.eoriNumbersVerification
                  .orElse(Some(EoriNumbersVerification()))
                  .map(_.copy(consigneeEoriNumber = Some(consigneeEoriNumber)))
              )
            )
          )
        else Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
        Right(
          this.copy(
            answers.copy(eoriNumbersVerification =
              answers.eoriNumbersVerification
                .orElse(Some(EoriNumbersVerification()))
                .map(_.copy(declarantEoriNumber = Some(declarantEoriNumber)))
            )
          )
        )
      else
        Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
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
      if (needsBanksAccountDetailsSubmission)
        Right(
          this.copy(
            answers.copy(bankAccountDetails =
              Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
            )
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          this.copy(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (getDocumentTypesIfRequired.exists(_.contains(documentType)))
        Right(
          this.copy(
            answers.copy(selectedDocumentType = Some(documentType))
          )
        )
      else
        Left("submitDocumentTypeSelection.invalid")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        if (
          getDocumentTypesIfRequired match {
            case Some(dts) => dts.contains(documentType)
            case None      => documentType === UploadDocumentType.SupportingEvidence
          }
        ) {
          val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
            case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
            case uf                            => uf
          }
          Right(
            this.copy(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
          )
        } else
          Left("receiveUploadedFiles.invalidDocumentType")
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

  def finalizeJourneyWith(caseNumber: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ => Right(new SecuritiesJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    obj match {
      case that: SecuritiesJourney =>
        that.answers === this.answers && that.caseNumber === this.caseNumber
      case _                       => false
    }

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"SecuritiesJourney($answers, $caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[Seq[String], SecuritiesJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for {
          mrn                 <- getLeadMovementReferenceNumber
          rfs                 <- getReasonForSecurity
          supportingEvidences  = answers.supportingEvidences
          claimantInformation <- getClaimantInformation
          payeeType           <- answers.payeeType
        } yield SecuritiesJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = payeeType,
          claimantInformation = claimantInformation,
          reasonForSecurity = rfs,
          securitiesReclaims = getSecuritiesReclaims,
          bankAccountDetails =
            if (needsBanksAccountDetailsSubmission)
              answers.bankAccountDetails
            else None,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          temporaryAdmissionMethodOfDisposal = answers.temporaryAdmissionMethodOfDisposal,
          exportMovementReferenceNumber = answers.exportMovementReferenceNumber
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
    new SecuritiesJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type CorrectedAmounts = SortedMap[TaxCode, Option[BigDecimal]]

  // so far empty but required by the JourneyCompanion interface
  final case class Features()

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    reasonForSecurity: Option[ReasonForSecurity] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    payeeType: Option[PayeeType] = None,
    similarClaimExistAlreadyInCDFPay: Option[Boolean] = None, // TPI04 check flag
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    exportMovementReferenceNumber: Option[MRN] =
      None, // mandatory for some reasons, see ReasonForSecurity.requiresExportDeclaration,
    temporaryAdmissionMethodOfDisposal: Option[TemporaryAdmissionMethodOfDisposal] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    correctedAmounts: Option[SortedMap[String, CorrectedAmounts]] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    modes: SecuritiesJourneyModes = SecuritiesJourneyModes()
  ) extends CommonAnswers {

    final override def checkYourAnswersChangeMode: Boolean =
      modes.checkYourAnswersChangeMode

    final override def enterContactDetailsMode: Boolean =
      modes.enterContactDetailsMode
  }

  final case class Output(
    movementReferenceNumber: MRN,
    payeeType: PayeeType,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    reasonForSecurity: ReasonForSecurity,
    securitiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]],
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument],
    temporaryAdmissionMethodOfDisposal: Option[TemporaryAdmissionMethodOfDisposal],
    exportMovementReferenceNumber: Option[MRN]
  )

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
          _.getTotalReclaimAmount > 0,
          TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO
        )

    val userCanProceedWithThisClaim: Validate[SecuritiesJourney] =
      hasMRNAndDisplayDeclarationAndRfS &
        thereIsNoSimilarClaimInCDFPay &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified

    val hasMethodOfDisposalIfNeeded: Validate[SecuritiesJourney] =
      conditionally[SecuritiesJourney](
        _.needsMethodOfDisposalSubmission,
        checkIsDefined(_.answers.temporaryAdmissionMethodOfDisposal, MISSING_METHOD_OF_DISPOSAL),
        checkIsEmpty(_.answers.temporaryAdmissionMethodOfDisposal, "unexpected method of disposal, should be empty")
      )

    val hasExportMRNIfNeeded: Validate[SecuritiesJourney] =
      conditionally[SecuritiesJourney](
        _.needsExportMRNSubmission,
        checkIsDefined(_.answers.exportMovementReferenceNumber, MISSING_EXPORT_MOVEMENT_REFERENCE_NUMBER),
        checkIsEmpty(_.answers.exportMovementReferenceNumber, "unexpected export MRN, should be empty")
      )
  }

  import Checks._

  override implicit val validator: Validate[SecuritiesJourney] =
    Validator.all(
      hasMRNAndDisplayDeclarationAndRfS,
      thereIsNoSimilarClaimInCDFPay,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      hasMethodOfDisposalIfNeeded,
      hasExportMRNIfNeeded,
      reclaimAmountsHasBeenDeclared,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided,
      payeeTypeIsDefined
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
        and (JsPath \ "caseNumber").readNullable[String])(new SecuritiesJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
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
      .flatMapWhenDefined(answers.temporaryAdmissionMethodOfDisposal)(_.submitTemporaryAdmissionMethodOfDisposal _)
      .flatMapWhenDefined(answers.exportMovementReferenceNumber)(_.submitExportMovementReferenceNumber _)
      .mapWhenDefined(answers.eoriNumbersVerification.flatMap(_.userXiEori))(_.submitUserXiEori _)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber))(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber))(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .flatMapEachWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectSecurityDepositId
      )
      .map(_.submitCheckDeclarationDetailsChangeMode(answers.modes.checkDeclarationDetailsChangeMode))
      .flatMapEachWhenDefined(answers.correctedAmounts)((journey: SecuritiesJourney) => {
        case (depositId: String, reclaims: SortedMap[TaxCode, Option[BigDecimal]]) =>
          journey
            .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, reclaims.keySet.toSeq)
            .flatMapEachWhenMappingDefined(reclaims)((journey: SecuritiesJourney) =>
              (taxCode: TaxCode, amount: BigDecimal) => {
                journey.submitCorrectAmount(depositId, taxCode, amount)
              }
            )
      })
      .map(_.submitClaimFullAmountMode(answers.modes.claimFullAmountMode))
      .map(_.submitCheckClaimDetailsChangeMode(answers.modes.checkClaimDetailsChangeMode))
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType _)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.getOrElse(UploadDocumentType.Other), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

}
