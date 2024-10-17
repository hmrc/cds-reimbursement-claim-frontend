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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

import java.time.LocalDateTime

/** An encapsulated C285 single MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[OverpaymentsSingleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[OverpaymentsSingleJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class OverpaymentsSingleJourney private (
  val answers: OverpaymentsSingleJourney.Answers,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[OverpaymentsSingleJourney.Features]
) extends JourneyBase
    with DirectFluentSyntax[OverpaymentsSingleJourney]
    with OverpaymentsJourneyProperties
    with SingleVariantProperties
    with JourneyAnalytics {

  type Type = OverpaymentsSingleJourney

  val self: OverpaymentsSingleJourney = this

  val validate: Validator.Validate[OverpaymentsSingleJourney] =
    OverpaymentsSingleJourney.validator

  private def copy(
    newAnswers: OverpaymentsSingleJourney.Answers
  ): OverpaymentsSingleJourney =
    new OverpaymentsSingleJourney(newAnswers, caseNumber, submissionDateTime, features)

  override def getAvailableClaimTypes(showDanOption: Boolean = false): Set[BasisOfOverpaymentClaim] =
    BasisOfOverpaymentClaim
      .excludeNorthernIrelandClaims(true, answers.displayDeclaration, hasDanOption = showDanOption)

  override def filterAvailableDuties(duties: Seq[(TaxCode, Boolean)]): Seq[(TaxCode, Boolean)] = {
    val wasIncorrectExciseCodeSelected: Boolean =
      answers.basisOfClaim.exists(_ === BasisOfOverpaymentClaim.IncorrectExciseValue)

    if (wasIncorrectExciseCodeSelected) {
      duties.filter { case (duty, _) =>
        TaxCodes.exciseTaxCodeSet.contains(duty)
      }
    } else {
      duties
    }
  }

  override def needsDocumentType: Boolean =
    features
      .map(feature => !feature.shouldSkipDocumentTypeSelection)
      .getOrElse(true)

  def needsDuplicateMrnAndDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry)

  def needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry) &&
      !(userHasGBEoriMatchingDuplicateDeclaration || userHasXIEoriMatchingDuplicateDeclaration)

  def getDuplicateDisplayDeclaration: Option[DisplayDeclaration] =
    answers.duplicateDeclaration
      .map(_.displayDeclaration)

  def getDeclarantEoriFromDuplicateACC14: Option[Eori] =
    getDuplicateDisplayDeclaration
      .map(_.getDeclarantEori)

  def getConsigneeEoriFromDuplicateACC14: Option[Eori] =
    getDuplicateDisplayDeclaration
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
      getDuplicateDisplayDeclaration.exists(_.containsXiEori) &&
      answers.eoriNumbersVerification.flatMap(_.userXiEori).isEmpty

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsSingleJourney =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): OverpaymentsSingleJourney =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  def removeUnsupportedTaxCodes(): OverpaymentsSingleJourney =
    this.copy(answers.copy(displayDeclaration = answers.displayDeclaration.map(_.removeUnsupportedTaxCodes())))

  def submitNewEori(eori: Eori): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newEori = Some(eori))
      )
    }

  def submitNewDan(dan: Dan): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newDan = Some(dan))
      )
    }

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn)
            if existingMrn === mrn &&
              getLeadDisplayDeclaration.contains(displayDeclaration) =>
          Right(this)
        case _ =>
          if (mrn =!= displayDeclaration.getMRN)
            Left(
              "submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
            )
          else
            Right(
              new OverpaymentsSingleJourney(
                OverpaymentsSingleJourney
                  .Answers(
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    displayDeclaration = Some(displayDeclaration),
                    eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly),
                    nonce = answers.nonce
                  ),
                features = features
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsSingleJourney] =
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
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
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      basisOfClaim match {
        case BasisOfOverpaymentClaim.DuplicateEntry =>
          this.copy(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case BasisOfOverpaymentClaim.IncorrectExciseValue =>
          this.copy(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              duplicateDeclaration = None,
              correctedAmounts = answers.correctedAmounts.map(_.filter { case (taxCode, _) =>
                TaxCodes.exciseTaxCodeSet.contains(taxCode)
              })
            )
          )

        case _ =>
          this.copy(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              duplicateDeclaration = None
            )
          )
      }
    }

  /** Resets the journey with the new MRN
    * or keep existing journey if submitted the same MRN and declaration as before.
    */
  def submitDuplicateMovementReferenceNumberAndDeclaration(
    duplicateMrn: MRN,
    duplicateDisplayDeclaration: DisplayDeclaration
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn) if existingMrn === duplicateMrn =>
          Left(
            "submitDuplicateMovementReferenceNumberAndDeclaration.mustBeDifferent"
          )
        case _                                                 =>
          if (duplicateMrn =!= duplicateDisplayDeclaration.getMRN)
            Left(
              "submitDuplicateMovementReferenceNumberAndDeclaration.wrongDisplayDeclarationMrn"
            )
          else {
            val modifiedJourney = this.copy(
              answers.copy(
                duplicateDeclaration = Some(DuplicateDeclaration(duplicateMrn, duplicateDisplayDeclaration))
              )
            )

            Right(
              this.copy(
                modifiedJourney.answers
                  .copy(duplicateDeclaration =
                    modifiedJourney.answers.duplicateDeclaration.map(
                      _.copy(verificationStatus =
                        if (modifiedJourney.needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration)
                          None
                        else
                          Some(DuplicateDeclarationVerificationStatus.verified)
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
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration)
        if (answers.duplicateDeclaration.flatMap(_.displayDeclaration.getConsigneeEori).contains(consigneeEoriNumber))
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
        else
          Left("checkConsigneeEoriNumberWithDuplicateDeclaration.shouldMatchConsigneeEoriFromACC14")
      else Right(this)
    }

  def checkDeclarantEoriNumberWithDuplicateDeclaration(
    declarantEoriNumber: Eori
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration)
        if (answers.duplicateDeclaration.map(_.displayDeclaration.getDeclarantEori).contains(declarantEoriNumber))
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
        else
          Left("checkDeclarantEoriNumberWithDuplicateDeclaration.shouldMatchDeclarantEoriFromACC14")
      else Right(this)
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      getLeadDisplayDeclaration match {
        case None => Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")

        case Some(_) =>
          if (taxCodes.isEmpty)
            Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(_).isDefined)
            if (allTaxCodesExistInACC14) {
              val newCorrectedAmounts = answers.correctedAmounts match {
                case None                      =>
                  Map(taxCodes.map(taxCode => taxCode -> None): _*)
                case Some(reimbursementClaims) =>
                  Map(taxCodes.map { taxCode =>
                    taxCode -> reimbursementClaims.get(taxCode).flatten
                  }: _*)
              }

              Right(
                this.copy(
                  if (!isAllSelectedDutiesAreCMAEligible(newCorrectedAmounts))
                    answers.copy(correctedAmounts = Some(newCorrectedAmounts), reimbursementMethod = None)
                  else
                    answers.copy(correctedAmounts = Some(newCorrectedAmounts))
                )
              )
            } else
              Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
          }
      }
    }

  def isValidCorrectAmount(correctAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    correctAmount >= 0 && correctAmount < BigDecimal(ndrcDetails.amount)

  def submitCorrectAmount(
    taxCode: TaxCode,
    correctAmount: ReimbursementClaim
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      getLeadDisplayDeclaration match {
        case None =>
          Left("submitCorrectAmount.missingDisplayDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(correctAmount.getAmount, ndrcDetails) =>
              if (getSelectedDuties.exists(_.contains(taxCode))) {
                val newCorrectedAmounts = answers.correctedAmounts match {
                  case None                   => Map(taxCode -> Some(correctAmount))
                  case Some(correctedAmounts) =>
                    correctedAmounts + (taxCode -> Some(correctAmount))
                }
                Right(this.copy(answers.copy(correctedAmounts = Some(newCorrectedAmounts))))
              } else
                Left("submitCorrectAmount.taxCodeNotSelectedYet")

            case _ =>
              Left("submitCorrectAmount.invalidAmount")
          }
      }
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, OverpaymentsSingleJourney] =
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsSingleJourney] =
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

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          this.copy(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitReimbursementMethod(
    reimbursementMethod: ReimbursementMethod
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (reimbursementMethod === ReimbursementMethod.CurrentMonthAdjustment) {
        if (isAllSelectedDutiesAreCMAEligible)
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = computeBankAccountDetails
              )
            )
          )
        else
          Left("submitReimbursementMethod.notCMAEligible")
      } else
        Right(
          this.copy(
            answers.copy(
              reimbursementMethod = Some(reimbursementMethod),
              bankAccountDetails = computeBankAccountDetails
            )
          )
        )
    }

  def resetReimbursementMethod(): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(
          reimbursementMethod = None,
          bankAccountType = None,
          bankAccountDetails = None
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = documentType)
          case uf                            => uf
        }
        Right(
          this.copy(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def submitNewEori(eori: Eori): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newEori = Some(eori))
      )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new OverpaymentsSingleJourney(
                answers = this.answers,
                caseNumber = Some(caseNumber),
                submissionDateTime = Some(LocalDateTime.now()),
                features = features
              )
            )
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[OverpaymentsSingleJourney]) {
      val that = obj.asInstanceOf[OverpaymentsSingleJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"OverpaymentsSingleJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[Seq[String], OverpaymentsSingleJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for {
          mrn                 <- getLeadMovementReferenceNumber
          basisOfClaim        <- answers.basisOfClaim
          additionalDetails   <- answers.additionalDetails
          supportingEvidences  =
            answers.supportingEvidences
              .map(file => if (file.documentType.isEmpty) file.copy(cargo = Some(UploadDocumentType.Other)) else file)
          claimantInformation <- getClaimantInformation
          payeeType           <- answers.payeeType
          newEoriAndDan        = (answers.newEori, answers.newDan) match {
                                   case (Some(newEori), Some(newDan)) => Some(NewEoriAndDan(newEori, newDan.value))
                                   case _                             => None
                                 }
        } yield OverpaymentsSingleJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = payeeType,
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

object OverpaymentsSingleJourney extends JourneyCompanion[OverpaymentsSingleJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): OverpaymentsSingleJourney =
    new OverpaymentsSingleJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce), features = features)

  type CorrectedAmounts = Map[TaxCode, Option[ReimbursementClaim]]

  final case class Features(
    shouldBlockSubsidies: Boolean,
    shouldAllowSubsidyOnlyPayments: Boolean,
    shouldSkipDocumentTypeSelection: Boolean
  ) extends SubsidiesFeatures

  // All user answers captured during C&E1179 single MRN journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    payeeType: Option[PayeeType] = None,
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
    modes: JourneyModes = JourneyModes()
  ) extends OverpaymentsAnswers
      with SingleVariantAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    duplicateMovementReferenceNumber: Option[MRN],
    claimantType: ClaimantType,
    payeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    reimbursements: Seq[Reimbursement],
    reimbursementMethod: ReimbursementMethod, //this has to stay for a while until we fully implement split payments
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument],
    newEoriAndDan: Option[NewEoriAndDan]
  )

  import com.github.arturopala.validator.Validator._
  import JourneyValidationErrors._

  object Checks extends OverpaymentsJourneyChecks[OverpaymentsSingleJourney] {

    val reimbursementMethodHasBeenProvidedIfNeeded: Validate[OverpaymentsSingleJourney] =
      all(
        whenTrue(
          j => j.isAllSelectedDutiesAreCMAEligible && !j.isSubsidyOnlyJourney,
          checkIsDefined(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_MUST_BE_DEFINED
          )
        ),
        whenTrue(
          _.isSubsidyOnlyJourney,
          checkIsEmpty(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED
          )
        )
      )

    val duplicateMovementReferenceNumberHasBeenProvidedIfNeeded: Validate[OverpaymentsSingleJourney] =
      all(
        whenTrue(
          _.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
          all(
            checkIsDefined(
              _.answers.duplicateDeclaration.map(_.movementReferenceNumber),
              DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED
            ),
            checkIsDefined(
              _.answers.duplicateDeclaration.map(_.displayDeclaration),
              DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED
            )
          )
        ),
        whenFalse(
          _.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
          all(
            checkIsEmpty(
              _.answers.duplicateDeclaration.map(_.movementReferenceNumber),
              DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_NOT_BE_DEFINED
            ),
            checkIsEmpty(
              _.answers.duplicateDeclaration.map(_.displayDeclaration),
              DUPLICATE_DISPLAY_DECLARATION_MUST_NOT_BE_DEFINED
            )
          )
        )
      )

    val needsDuplicateMrnAndDeclaration: Validate[OverpaymentsSingleJourney] =
      checkIsTrue(
        journey => journey.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
        DUPLICATE_MOVEMENT_REFERENCE_NUMBER_NOT_REQUIRED
      )

    val hasDuplicateMovementReferenceNumber: Validate[OverpaymentsSingleJourney] =
      checkIsTrue(
        journey => journey.answers.duplicateDeclaration.map(_.movementReferenceNumber).isDefined,
        DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED
      )

    val hasDuplicateDisplayDeclaration: Validate[OverpaymentsSingleJourney] =
      checkIsTrue(
        journey => journey.answers.duplicateDeclaration.map(_.displayDeclaration).isDefined,
        DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED
      )

    val hasDuplicateDisplayDeclarationVerified: Validate[OverpaymentsSingleJourney] =
      checkIsTrue(
        journey => journey.answers.duplicateDeclaration.flatMap(_.verificationStatus).exists(_.isVerified),
        DUPLICATE_DISPLAY_DECLARATION_MUST_BE_VERIFIED
      )

    val hasDuplicateMRNAndDisplayDeclaration: Validate[OverpaymentsSingleJourney] =
      hasDuplicateMovementReferenceNumber & hasDuplicateDisplayDeclaration

    val hasDuplicateDeclarationVerifiedIfRequired: Validate[OverpaymentsSingleJourney] =
      conditionally(
        _.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
        hasDuplicateMRNAndDisplayDeclaration andWhenValid hasDuplicateDisplayDeclarationVerified,
        always[OverpaymentsSingleJourney]
      )

    val changeDutiesModeDisabled: Validate[OverpaymentsSingleJourney] =
      checkIsFalse(
        _.answers.modes.dutiesChangeMode,
        DUTIES_CHANGE_MODE_ENABLED
      )

  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsSingleJourney] =
    all(
      hasMRNAndDisplayDeclaration,
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
      whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments,
      payeeTypeIsDefined
    )

  import JourneyFormats._

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

  implicit val format: Format[OverpaymentsSingleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new OverpaymentsSingleJourney(_, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(journey =>
        (journey.answers, journey.caseNumber, journey.submissionDateTime, journey.features)
      )
    )

  /** Try to build journey from the pre-existing answers. */
  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, OverpaymentsSingleJourney] =
    empty(answers.userEoriNumber, answers.nonce, features)
      .flatMapWhenDefined(
        answers.movementReferenceNumber.zip(answers.displayDeclaration)
      )(j => { case (mrn: MRN, decl: DisplayDeclaration) =>
        j.submitMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .mapWhenDefined(answers.eoriNumbersVerification.flatMap(_.userXiEori))(_.submitUserXiEori _)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber))(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber))(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .flatMapWhenDefined(
        answers.duplicateDeclaration
          .map(_.movementReferenceNumber)
          .zip(answers.duplicateDeclaration.map(_.displayDeclaration))
      )(j => { case (mrn: MRN, decl: DisplayDeclaration) =>
        j.submitDuplicateMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .flatMapWhenDefined(answers.duplicateDeclaration.map(_.displayDeclaration))(j =>
        d => d.getConsigneeEori.map(e => j.checkConsigneeEoriNumberWithDuplicateDeclaration(e)).getOrElse(Right(j))
      )
      .flatMapWhenDefined(answers.duplicateDeclaration.map(_.displayDeclaration))(j =>
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
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType _)
      .mapWhenDefined(answers.newEori)(_.submitNewEori)
      .mapWhenDefined(answers.newDan)(_.submitNewDan)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

}
