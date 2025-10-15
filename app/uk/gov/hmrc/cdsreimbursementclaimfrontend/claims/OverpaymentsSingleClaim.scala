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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import cats.syntax.eq.*
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator

import java.time.Instant
import java.time.LocalDateTime

/** An encapsulated C285 single MRN claim logic. The constructor of this class MUST stay PRIVATE to protected integrity
  * of the claim.
  *
  * The claim uses two nested case classes:
  *
  *   - [[OverpaymentsSingleClaim.Answers]] - keeps record of user answers and acquired documents
  *   - [[OverpaymentsSingleClaim.Output]] - final output of the claim to be sent to backend processing
  */
final class OverpaymentsSingleClaim private (
  val answers: OverpaymentsSingleClaim.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[OverpaymentsSingleClaim.Features]
) extends ClaimBase
    with DirectFluentSyntax[OverpaymentsSingleClaim]
    with OverpaymentsClaimProperties
    with SingleVariantProperties
    with ClaimAnalytics {

  type Type = OverpaymentsSingleClaim

  val self: OverpaymentsSingleClaim = this

  val validate: Validator.Validate[OverpaymentsSingleClaim] =
    OverpaymentsSingleClaim.validator

  private def copy(
    newAnswers: OverpaymentsSingleClaim.Answers
  ): OverpaymentsSingleClaim =
    new OverpaymentsSingleClaim(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  override def getAvailableClaimTypes: Set[BasisOfOverpaymentClaim] =
    BasisOfOverpaymentClaim
      .excludeNorthernIrelandClaims(
        true,
        answers.importDeclaration,
        isOtherEnabled = features.exists(_.shouldAllowOtherBasisOfClaim)
      )

  override def filterAvailableDuties(duties: Seq[(TaxCode, Boolean)]): Seq[(TaxCode, Boolean)] = {
    val wasIncorrectExciseCodeSelected: Boolean =
      answers.basisOfClaim.exists(_ === BasisOfOverpaymentClaim.IncorrectExciseValue)

    if wasIncorrectExciseCodeSelected then {
      duties.filter { case (duty, _) =>
        TaxCodes.exciseTaxCodeSet.contains(duty)
      }
    } else {
      duties
    }
  }

  def needsDuplicateMrnAndDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry)

  def needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry) &&
      !(userHasGBEoriMatchingDuplicateDeclaration || userHasXIEoriMatchingDuplicateDeclaration)

  def getDuplicateImportDeclaration: Option[ImportDeclaration] =
    answers.duplicateDeclaration
      .map(_.importDeclaration)

  def getDeclarantEoriFromDuplicateACC14: Option[Eori] =
    getDuplicateImportDeclaration
      .map(_.getDeclarantEori)

  def getConsigneeEoriFromDuplicateACC14: Option[Eori] =
    getDuplicateImportDeclaration
      .flatMap(_.getConsigneeEori)

  def userHasGBEoriMatchingDuplicateDeclaration: Boolean =
    getDeclarantEoriFromDuplicateACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromDuplicateACC14.contains(answers.userEoriNumber)

  def userHasXIEoriMatchingDuplicateDeclaration: Boolean =
    answers.eoriNumbersVerification.exists(x =>
      x.hasSameXiEoriAs(getDeclarantEoriFromDuplicateACC14) ||
        x.hasSameXiEoriAs(getConsigneeEoriFromDuplicateACC14)
    )

  def needsUserXiEoriSubmissionForDuplicateDeclaration: Boolean =
    !userHasGBEoriMatchingDuplicateDeclaration &&
      getDuplicateImportDeclaration.exists(_.containsXiEori) &&
      answers.eoriNumbersVerification.flatMap(_.userXiEori).isEmpty

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsSingleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): OverpaymentsSingleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  def removeUnsupportedTaxCodes(): OverpaymentsSingleClaim =
    this.copy(answers.copy(importDeclaration = answers.importDeclaration.map(_.removeUnsupportedTaxCodes())))

  def submitNewEori(eori: Eori): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newEori = Some(eori))
      )
    }

  def submitNewDan(dan: Dan): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newDan = Some(dan))
      )
    }

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn)
            if existingMrn === mrn &&
              getLeadImportDeclaration.contains(importDeclaration) =>
          Right(this)
        case _ =>
          if mrn =!= importDeclaration.getMRN then
            Left(
              "submitMovementReferenceNumber.wrongImportDeclarationMrn"
            )
          else
            Right(
              new OverpaymentsSingleClaim(
                OverpaymentsSingleClaim
                  .Answers(
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    importDeclaration = Some(importDeclaration),
                    eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly),
                    nonce = answers.nonce
                  ),
                startTimeSeconds = this.startTimeSeconds,
                features = features
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsSingleClaim] =
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
        else Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if needsDeclarantAndConsigneeEoriSubmission then
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
        else Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      basisOfClaim match {
        case BasisOfOverpaymentClaim.DuplicateEntry =>
          this.copy(answers.copy(basisOfClaim = Some(basisOfClaim), newEori = None, newDan = None))

        case BasisOfOverpaymentClaim.IncorrectEoriAndDan if answers.basisOfClaim.contains(IncorrectEoriAndDan) =>
          this.copy(answers.copy(basisOfClaim = Some(basisOfClaim), duplicateDeclaration = None))

        case BasisOfOverpaymentClaim.IncorrectExciseValue =>
          this.copy(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              duplicateDeclaration = None,
              correctedAmounts = answers.correctedAmounts.map(_.filter { case (taxCode, _) =>
                TaxCodes.exciseTaxCodeSet.contains(taxCode)
              }),
              newEori = None,
              newDan = None
            )
          )

        case _ =>
          this.copy(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              duplicateDeclaration = None,
              newEori = None,
              newDan = None
            )
          )
      }
    }

  /** Resets the claim with the new MRN or keep existing claim if submitted the same MRN and declaration as before.
    */
  def submitDuplicateMovementReferenceNumberAndDeclaration(
    duplicateMrn: MRN,
    duplicateImportDeclaration: ImportDeclaration
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn) if existingMrn === duplicateMrn =>
          Left(
            "submitDuplicateMovementReferenceNumberAndDeclaration.mustBeDifferent"
          )
        case _                                                 =>
          if duplicateMrn =!= duplicateImportDeclaration.getMRN then
            Left(
              "submitDuplicateMovementReferenceNumberAndDeclaration.wrongImportDeclarationMrn"
            )
          else {
            val modifiedClaim = this.copy(
              answers.copy(
                duplicateDeclaration = Some(DuplicateDeclaration(duplicateMrn, duplicateImportDeclaration))
              )
            )

            Right(
              this.copy(
                modifiedClaim.answers
                  .copy(duplicateDeclaration =
                    modifiedClaim.answers.duplicateDeclaration.map(
                      _.copy(verificationStatus =
                        if modifiedClaim.needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration then None
                        else Some(DuplicateDeclarationVerificationStatus.verified)
                      )
                    )
                  )
              )
            )
          }
      }
    }

  def checkConsigneeEoriNumberWithDuplicateDeclaration(
    consigneeEoriNumber: Eori
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration then
        if answers.duplicateDeclaration.flatMap(_.importDeclaration.getConsigneeEori).contains(consigneeEoriNumber)
        then
          Right(
            this.copy(
              answers.copy(duplicateDeclaration =
                answers.duplicateDeclaration.map(
                  _.copy(verificationStatus =
                    Some(
                      answers.duplicateDeclaration
                        .flatMap(_.verificationStatus)
                        .getOrElse(DuplicateDeclarationVerificationStatus.unverified)
                        .withConsigneeEoriVerified
                    )
                  )
                )
              )
            )
          )
        else Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DUPLICATE_CONSIGNEE_EORI)
      else Right(this)
    }

  def checkDeclarantEoriNumberWithDuplicateDeclaration(
    declarantEoriNumber: Eori
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration then
        if answers.duplicateDeclaration.map(_.importDeclaration.getDeclarantEori).contains(declarantEoriNumber) then
          Right(
            this.copy(
              answers.copy(duplicateDeclaration =
                answers.duplicateDeclaration.map(
                  _.copy(verificationStatus =
                    Some(
                      answers.duplicateDeclaration
                        .flatMap(_.verificationStatus)
                        .getOrElse(DuplicateDeclarationVerificationStatus.unverified)
                        .withDeclarantEoriVerified
                    )
                  )
                )
              )
            )
          )
        else Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DUPLICATE_DECLARANT_EORI)
      else Right(this)
    }

  def needsCurrencyTypeSelection: Boolean =
    getSelectedDuties.exists(_.size % 2 == 0)

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      getLeadImportDeclaration match {
        case None => Left("selectTaxCodeSetForReimbursement.missingImportDeclaration")

        case Some(_) =>
          if taxCodes.isEmpty then Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(_).isDefined)
            if allTaxCodesExistInACC14 then {
              val newCorrectedAmounts = answers.correctedAmounts match {
                case None                      =>
                  Map(taxCodes.map(taxCode => taxCode -> None)*)
                case Some(reimbursementClaims) =>
                  Map(taxCodes.map { taxCode =>
                    taxCode -> reimbursementClaims.get(taxCode).flatten
                  }*)
              }

              Right(
                this.copy(
                  if !isAllSelectedDutiesAreCMAEligible(newCorrectedAmounts) then
                    answers.copy(correctedAmounts = Some(newCorrectedAmounts), reimbursementMethod = None)
                  else answers.copy(correctedAmounts = Some(newCorrectedAmounts))
                )
              )
            } else Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
          }
      }
    }

  def isValidCorrectAmount(correctAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    correctAmount >= 0 && correctAmount < BigDecimal(ndrcDetails.amount)

  def submitCorrectAmount(
    taxCode: TaxCode,
    correctAmount: BigDecimal
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      getLeadImportDeclaration match {
        case None =>
          Left("submitCorrectAmount.missingImportDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(correctAmount, ndrcDetails) =>
              if getSelectedDuties.exists(_.contains(taxCode)) then {
                val newCorrectedAmounts = answers.correctedAmounts.get + (taxCode -> Some(correctAmount))
                Right(this.copy(answers.copy(correctedAmounts = Some(newCorrectedAmounts))))
              } else Left("submitCorrectAmount.taxCodeNotSelectedYet")

            case Some(ndrcDetails) =>
              Left("submitCorrectAmount.invalidAmount")
          }
      }
    }

  def submitClaimAmount(
    taxCode: TaxCode,
    claimAmount: BigDecimal
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      getLeadImportDeclaration match {
        case None =>
          Left("submitCorrectAmount.missingImportDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(BigDecimal(ndrcDetails.amount) - claimAmount, ndrcDetails) =>
              if getSelectedDuties.exists(_.contains(taxCode)) then {
                val correctAmount       = BigDecimal(ndrcDetails.amount) - claimAmount
                val newCorrectedAmounts = answers.correctedAmounts.get + (taxCode -> Some(correctAmount))
                Right(this.copy(answers.copy(correctedAmounts = Some(newCorrectedAmounts))))
              } else Left("submitCorrectAmount.taxCodeNotSelectedYet")

            case _ =>
              Left("submitCorrectAmount.invalidAmount")
          }
      }
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if answers.payeeType.contains(payeeType) then Right(this)
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

  def submitCurrencyType(currencyType: CurrencyType): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if needsCurrencyTypeSelection
      then
        if answers.currencyType.contains(currencyType)
        then Right(this)
        else
          Right(
            copy(newAnswers =
              answers.copy(
                currencyType = Some(currencyType)
              )
            )
          )
      else Left("submitCurrencyType.doesNotNeedCurrencyTypeSelection")
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountDetails =
            Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
          )
        )
      )
    }

  def removeBankAccountDetails(): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitReimbursementMethod(
    reimbursementMethod: ReimbursementMethod
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if reimbursementMethod === ReimbursementMethod.CurrentMonthAdjustment then {
        if isAllSelectedDutiesAreCMAEligible then
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod)
              )
            )
          )
        else Left("submitReimbursementMethod.notCMAEligible")
      } else
        Right(
          this.copy(
            answers.copy(
              reimbursementMethod = Some(reimbursementMethod)
            )
          )
        )
    }

  def resetReimbursementMethod(): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(
          reimbursementMethod = None,
          bankAccountType = None,
          bankAccountDetails = None
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = documentType)
          case uf                            => uf
        }
        Right(
          this.copy(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsSingleClaim =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeClaimWith(caseNumber: String): Either[String, OverpaymentsSingleClaim] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new OverpaymentsSingleClaim(
                answers = this.answers,
                startTimeSeconds = this.startTimeSeconds,
                caseNumber = Some(caseNumber),
                submissionDateTime = Some(LocalDateTime.now()),
                features = features
              )
            )
        )
    }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[OverpaymentsSingleClaim] then {
      val that = obj.asInstanceOf[OverpaymentsSingleClaim]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"OverpaymentsSingleClaim${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the claim and retrieves the output. */

  def toOutput: Either[Seq[String], OverpaymentsSingleClaim.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for
          mrn                 <- getLeadMovementReferenceNumber
          basisOfClaim        <- answers.basisOfClaim
          additionalDetails   <- answers.additionalDetails
          supportingEvidences  =
            answers.supportingEvidences
              .map(file =>
                if file.documentType.isEmpty then file.copy(cargo = Some(UploadDocumentType.Other)) else file
              )
          claimantInformation <- getClaimantInformation
          payeeType           <- getPayeeTypeForOutput(answers.payeeType)
          displayPayeeType    <- answers.payeeType
          newEoriAndDan        = (basisOfClaim, answers.newEori, answers.newDan) match {
                                   case (IncorrectEoriAndDan, Some(newEori), Some(newDan)) =>
                                     Some(NewEoriAndDan(newEori, newDan.value))
                                   case _                                                  => None
                                 }
        yield OverpaymentsSingleClaim.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = payeeType,
          displayPayeeType = displayPayeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          additionalDetails = additionalDetails,
          reimbursements = getReimbursements,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          duplicateMovementReferenceNumber = answers.duplicateDeclaration.map(_.movementReferenceNumber),
          reimbursementMethod = getDefaultReimbursementMethod,
          bankAccountDetails = answers.bankAccountDetails,
          newEoriAndDan = newEoriAndDan
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object OverpaymentsSingleClaim extends ClaimCompanion[OverpaymentsSingleClaim] {

  /** A starting point to build new instance of the claim. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): OverpaymentsSingleClaim =
    new OverpaymentsSingleClaim(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = Map[TaxCode, Option[BigDecimal]]

  final case class Features(
    shouldAllowOtherBasisOfClaim: Boolean
  )

  // All user answers captured during C&E1179 single MRN claim
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    importDeclaration: Option[ImportDeclaration] = None,
    payeeType: Option[PayeeType] = None,
    currencyType: Option[CurrencyType] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    duplicateDeclaration: Option[DuplicateDeclaration] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfOverpaymentClaim] = None,
    additionalDetails: Option[String] = None,
    correctedAmounts: Option[CorrectedAmounts] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    reimbursementMethod: Option[ReimbursementMethod] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    newEori: Option[Eori] = None,
    newDan: Option[Dan] = None,
    modes: ClaimModes = ClaimModes()
  ) extends OverpaymentsAnswers
      with SingleVariantAnswers

  // Final minimal output of the claim we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    duplicateMovementReferenceNumber: Option[MRN],
    claimantType: ClaimantType,
    payeeType: PayeeType,
    displayPayeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    reimbursements: Seq[Reimbursement],
    reimbursementMethod: ReimbursementMethod, // this has to stay for a while until we fully implement split payments
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument],
    newEoriAndDan: Option[NewEoriAndDan]
  ) extends WafErrorMitigation[Output] {

    override def excludeFreeTextInputs() =
      (
        Seq(("additional_details", additionalDetails)),
        this.copy(additionalDetails = additionalDetailsReplacementText)
      )
  }

  import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator._
  import ClaimValidationErrors._

  object Checks extends OverpaymentsClaimChecks[OverpaymentsSingleClaim] {

    val reimbursementMethodHasBeenProvidedIfNeeded: Validate[OverpaymentsSingleClaim] =
      all(
        whenTrue(
          j => j.isAllSelectedDutiesAreCMAEligible,
          checkIsDefined(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_MUST_BE_DEFINED
          )
        )
      )

    val duplicateMovementReferenceNumberHasBeenProvidedIfNeeded: Validate[OverpaymentsSingleClaim] =
      all(
        whenTrue(
          _.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
          all(
            checkIsDefined(
              _.answers.duplicateDeclaration.map(_.movementReferenceNumber),
              DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED
            ),
            checkIsDefined(
              _.answers.duplicateDeclaration.map(_.importDeclaration),
              DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED
            )
          )
        )
      )

    val needsDuplicateMrnAndDeclaration: Validate[OverpaymentsSingleClaim] =
      checkIsTrue(
        claim => claim.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
        DUPLICATE_MOVEMENT_REFERENCE_NUMBER_NOT_REQUIRED
      )

    val hasDuplicateMovementReferenceNumber: Validate[OverpaymentsSingleClaim] =
      checkIsTrue(
        claim => claim.answers.duplicateDeclaration.map(_.movementReferenceNumber).isDefined,
        DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED
      )

    val hasDuplicateImportDeclaration: Validate[OverpaymentsSingleClaim] =
      checkIsTrue(
        claim => claim.answers.duplicateDeclaration.map(_.importDeclaration).isDefined,
        DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED
      )

    val hasDuplicateImportDeclarationVerified: Validate[OverpaymentsSingleClaim] =
      checkIsTrue(
        claim => claim.answers.duplicateDeclaration.flatMap(_.verificationStatus).exists(_.isVerified),
        DUPLICATE_DISPLAY_DECLARATION_MUST_BE_VERIFIED
      )

    val hasDuplicateMRNAndImportDeclaration: Validate[OverpaymentsSingleClaim] =
      hasDuplicateMovementReferenceNumber & hasDuplicateImportDeclaration

    val hasDuplicateDeclarationVerifiedIfRequired: Validate[OverpaymentsSingleClaim] =
      conditionally(
        _.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
        hasDuplicateMRNAndImportDeclaration andWhenValid hasDuplicateImportDeclarationVerified,
        always[OverpaymentsSingleClaim]
      )

    val changeDutiesModeDisabled: Validate[OverpaymentsSingleClaim] =
      checkIsFalse(
        _.answers.modes.dutiesChangeMode,
        DUTIES_CHANGE_MODE_ENABLED
      )

  }

  import Checks._

  /** Validate if all required answers has been provided and the claim is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsSingleClaim] =
    all(
      hasMRNAndImportDeclaration,
      containsOnlySupportedTaxCodes,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      hasDuplicateDeclarationVerifiedIfRequired,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      changeDutiesModeDisabled,
      reimbursementMethodHasBeenProvidedIfNeeded,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided,
      declarationsHasNoSubsidyPayments,
      payeeTypeIsDefined,
      newEoriAndDanProvidedIfNeeded,
      duplicateMovementReferenceNumberHasBeenProvidedIfNeeded
    )

  import ClaimFormats._

  object Features {
    implicit val format: Format[Features] =
      Json.using[Json.WithDefaultValues].format[Features]
  }

  object Answers {

    implicit val format: Format[Answers] =
      Json.using[Json.WithDefaultValues].format[Answers]
  }

  object Output {
    implicit val format: Format[Output] = Json.format[Output]
  }

  import play.api.libs.functional.syntax._

  implicit val format: Format[OverpaymentsSingleClaim] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new OverpaymentsSingleClaim(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(claim =>
        (claim.answers, claim.startTimeSeconds, claim.caseNumber, claim.submissionDateTime, claim.features)
      )
    )

  /** Try to build claim from the pre-existing answers. */
  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, OverpaymentsSingleClaim] =
    empty(answers.userEoriNumber, answers.nonce, features)
      .flatMapWhenDefined(
        answers.movementReferenceNumber.zip(answers.importDeclaration)
      )(j => { case (mrn: MRN, decl: ImportDeclaration) =>
        j.submitMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .mapWhenDefined(answers.eoriNumbersVerification.flatMap(_.userXiEori))(_.submitUserXiEori)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber))(_.submitConsigneeEoriNumber)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber))(_.submitDeclarantEoriNumber)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .flatMapWhenDefined(
        answers.duplicateDeclaration
          .map(_.movementReferenceNumber)
          .zip(answers.duplicateDeclaration.map(_.importDeclaration))
      )(j => { case (mrn: MRN, decl: ImportDeclaration) =>
        j.submitDuplicateMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .flatMapWhenDefined(answers.duplicateDeclaration.map(_.importDeclaration))(j =>
        d => d.getConsigneeEori.map(e => j.checkConsigneeEoriNumberWithDuplicateDeclaration(e)).getOrElse(Right(j))
      )
      .flatMapWhenDefined(answers.duplicateDeclaration.map(_.importDeclaration))(j =>
        d => j.checkDeclarantEoriNumberWithDuplicateDeclaration(d.getDeclarantEori)
      )
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectAndReplaceTaxCodeSetForReimbursement
      )
      .flatMapEachWhenDefinedAndMappingDefined(answers.correctedAmounts)(_.submitCorrectAmount)
      .map(_.withDutiesChangeMode(answers.modes.dutiesChangeMode))
      .flatMapWhenDefined(answers.reimbursementMethod)(_.submitReimbursementMethod)
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
      .flatMapWhenDefined(answers.currencyType)(_.submitCurrencyType)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType)
      .mapWhenDefined(answers.newEori)(_.submitNewEori)
      .mapWhenDefined(answers.newDan)(_.submitNewDan)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

  /** This method MUST BE used only to test the validation correctness of the invalid answer states. */
  def unsafeModifyAnswers(
    claim: OverpaymentsSingleClaim,
    f: OverpaymentsSingleClaim.Answers => OverpaymentsSingleClaim.Answers
  ): OverpaymentsSingleClaim =
    OverpaymentsSingleClaim(
      answers = f(claim.answers),
      startTimeSeconds = claim.startTimeSeconds,
      features = claim.features
    )

}
