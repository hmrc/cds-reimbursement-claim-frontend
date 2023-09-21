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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.CorrectedAmounts
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.CurrentMonthAdjustment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

/** An encapsulated C&E1179 single MRN journey logic.
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
  @annotation.nowarn val features: Option[OverpaymentsSingleJourney.Features] = None
) extends JourneyBase
    with DirectFluentSyntax[OverpaymentsSingleJourney]
    with OverpaymentsJourneyProperties
    with CanSubmitMrnAndDeclaration
    with CanSubmitContactDetails
    with JourneyAnalytics {

  type Type = OverpaymentsSingleJourney

  val self: OverpaymentsSingleJourney = this

  val validate: Validator.Validate[OverpaymentsSingleJourney] =
    OverpaymentsSingleJourney.validator

  private def copy(
    newAnswers: OverpaymentsSingleJourney.Answers
  ): OverpaymentsSingleJourney =
    new OverpaymentsSingleJourney(newAnswers, caseNumber, features)

  /** Check if all the selected duties have correct amounts provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.correctedAmounts.exists(rc => rc.nonEmpty && rc.forall(_._2.isDefined))

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean =
    !isSubsidyOnlyJourney &&
      answers.reimbursementMethod.isEmpty ||
      answers.reimbursementMethod.contains(ReimbursementMethod.BankAccountTransfer)

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

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getAvailableDuties: Seq[(TaxCode, Boolean)] = {

    val duties = getNdrcDetails
      .flatMap { ndrcs =>
        val taxCodes = ndrcs
          .map(ndrc =>
            TaxCodes
              .find(ndrc.taxType)
              .map(taxCode => (taxCode, ndrc.isCmaEligible))
          )
          .collect { case Some(x) => x }
        if (taxCodes.isEmpty) None else Some(taxCodes)
      }
      .getOrElse(Seq.empty)

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

  def getSelectedDuties: Option[Seq[TaxCode]] =
    answers.correctedAmounts.map(_.keys.toSeq)

  def isTaxCodeSelected(taxCode: TaxCode): Boolean =
    answers.correctedAmounts.exists(_.contains(taxCode))

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.correctedAmounts.exists(isAllSelectedDutiesAreCMAEligible)

  def isAllSelectedDutiesAreCMAEligible(amounts: CorrectedAmounts): Boolean =
    amounts.keySet
      .map(getNdrcDetailsFor)
      .collect { case Some(d) => d }
      .forall(_.isCmaEligible)

  def getNextNdrcDetailsToClaim: Option[NdrcDetails] =
    answers.correctedAmounts
      .flatMap(
        _.collectFirst { case (taxCode, None) => taxCode }
          .flatMap(getNdrcDetailsFor)
      )

  def getSelectedTaxCodesWithCorrectAmount: Seq[(TaxCode, BigDecimal)] =
    answers.correctedAmounts
      .map(
        _.collect { case (taxCode, Some(correctAmount)) => (taxCode, correctAmount) }.toSeq
      )
      .getOrElse(Seq.empty)

  def getAvailableTaxCodesWithPaidAmounts: Seq[(TaxCode, BigDecimal)] =
    getLeadDisplayDeclaration
      .flatMap(_.getNdrcDutiesWithAmount)
      .getOrElse(Seq.empty)

  def getReimbursementClaims: Map[TaxCode, BigDecimal] = {
    val taxCodesWithPaidAmounts: Map[TaxCode, BigDecimal] =
      getAvailableTaxCodesWithPaidAmounts.toMap

    answers.correctedAmounts
      .map(
        _.map { case (taxCode, correctAmountOpt) =>
          for {
            correctAmount <- correctAmountOpt
            paidAmount    <- taxCodesWithPaidAmounts.get(taxCode)
          } yield (taxCode, paidAmount - correctAmount)
        }.collect { case Some(x) => x }.toMap
      )
      .getOrElse(Map.empty)
  }

  def getUKDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(TaxCodes.ukTaxCodeSet)

  def getEUDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(TaxCodes.euTaxCodeSet)

  def getExciseDutyReimbursementTotal: Option[BigDecimal] =
    getReimbursementTotalBy(TaxCodes.exciseTaxCodeSet)

  private def getReimbursementTotalBy(include: TaxCode => Boolean): Option[BigDecimal] =
    getReimbursementClaims.foldLeft[Option[BigDecimal]](None) { case (a, (taxCode, amount)) =>
      if (include(taxCode)) Some(a.getOrElse(BigDecimal("0.00")) + amount)
      else a
    }

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.toSeq.map(_._2).sum

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsSingleJourney =
    this.copy(answers.copy(dutiesChangeMode = enabled))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsSingleDocumentTypes)

  override def getAvailableClaimTypes: Set[BasisOfOverpaymentClaim] =
    BasisOfOverpaymentClaim
      .excludeNorthernIrelandClaims(true, answers.whetherNorthernIreland.getOrElse(false), answers.displayDeclaration)

  def hasCmaReimbursementMethod =
    answers.reimbursementMethod.contains(CurrentMonthAdjustment)

  /** Resets the journey with the new MRN
    * or keep existing journey if submitted the same MRN and declaration as before.
    */
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

  override def computeBankAccountDetails: Option[BankAccountDetails] =
    answers.bankAccountDetails match {
      case Some(details) => Some(details)
      case None          =>
        val maybeDeclarantBankDetails       = getDeclarantBankAccountDetails
        val maybeConsigneeBankDetails       = getConsigneeBankAccountDetails
        val consigneeAndDeclarantEorisMatch = (for {
          consigneeEori <- getConsigneeEoriFromACC14
          declarantEori <- getDeclarantEoriFromACC14
        } yield consigneeEori === declarantEori).getOrElse(false)

        (answers.payeeType, maybeDeclarantBankDetails, maybeConsigneeBankDetails) match {
          case (Some(PayeeType.Consignee), _, Some(consigneeBankDetails))                                       =>
            Some(consigneeBankDetails)
          case (Some(PayeeType.Declarant), Some(declarantBankDetails), _)                                       =>
            Some(declarantBankDetails)
          case (Some(PayeeType.Declarant), None, Some(consigneeBankDetails)) if consigneeAndDeclarantEorisMatch =>
            Some(consigneeBankDetails)
          case (Some(PayeeType.Consignee), Some(declarantBankDetails), None) if consigneeAndDeclarantEorisMatch =>
            Some(declarantBankDetails)
          case _                                                                                                => None
        }
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

  def submitWhetherNorthernIreland(whetherNorthernIreland: Boolean): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(
          whetherNorthernIreland = Some(whetherNorthernIreland),
          basisOfClaim =
            if (whetherNorthernIreland) answers.basisOfClaim
            else
              // review basis of claim if nothern ireland claims should not be allowed
              answers.basisOfClaim.flatMap { case basisOfClaim =>
                if (BasisOfOverpaymentClaim.northernIreland.contains(basisOfClaim)) None
                else Some(basisOfClaim)
              }
        )
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
    correctAmount: BigDecimal
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      getLeadDisplayDeclaration match {
        case None =>
          Left("submitCorrectAmount.missingDisplayDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(correctAmount, ndrcDetails) =>
              if (getSelectedDuties.exists(_.contains(taxCode))) {
                val newCorrectedAmounts = answers.correctedAmounts match {
                  case None                   => Map(taxCode -> Some(correctAmount))
                  case Some(correctedAmounts) => correctedAmounts + (taxCode -> Some(correctAmount))
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
      if (isAllSelectedDutiesAreCMAEligible) {
        if (reimbursementMethod === ReimbursementMethod.CurrentMonthAdjustment)
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = None
              )
            )
          )
        else
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = computeBankAccountDetails
              )
            )
          )
      } else
        Left("submitReimbursementMethod.notCMAEligible")
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
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
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
          _ => this.copy(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new OverpaymentsSingleJourney(answers = this.answers, caseNumber = Some(caseNumber), features = features)
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
          mrn                    <- getLeadMovementReferenceNumber
          basisOfClaim           <- answers.basisOfClaim
          additionalDetails      <- answers.additionalDetails
          supportingEvidences     = answers.supportingEvidences
          claimantInformation    <- getClaimantInformation
          whetherNorthernIreland <- answers.whetherNorthernIreland
          payeeType              <- answers.payeeType
        } yield OverpaymentsSingleJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = payeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          whetherNorthernIreland = whetherNorthernIreland,
          additionalDetails = additionalDetails,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          duplicateMovementReferenceNumber = answers.duplicateDeclaration.map(_.movementReferenceNumber),
          reimbursementMethod = answers.reimbursementMethod.getOrElse(ReimbursementMethod.BankAccountTransfer),
          bankAccountDetails = answers.bankAccountDetails
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

  type CorrectedAmounts = Map[TaxCode, Option[BigDecimal]]

  final case class Features(
    shouldBlockSubsidies: Boolean,
    shouldAllowSubsidyOnlyPayments: Boolean
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
    whetherNorthernIreland: Option[Boolean] = None,
    additionalDetails: Option[String] = None,
    correctedAmounts: Option[CorrectedAmounts] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    reimbursementMethod: Option[ReimbursementMethod] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = false,
    dutiesChangeMode: Boolean = false
  ) extends OverpaymentsAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    duplicateMovementReferenceNumber: Option[MRN],
    claimantType: ClaimantType,
    payeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    whetherNorthernIreland: Boolean,
    additionalDetails: String,
    reimbursementClaims: Map[TaxCode, BigDecimal],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
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
        whenFalse(
          _.isAllSelectedDutiesAreCMAEligible,
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
        _.answers.dutiesChangeMode,
        DUTIES_CHANGE_MODE_ENABLED
      )

  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsSingleJourney] =
    all(
      hasMRNAndDisplayDeclaration,
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
      whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments
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
        and (JsPath \ "features").readNullable[Features])(new OverpaymentsSingleJourney(_, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "features").writeNullable[Features])(journey =>
        (journey.answers, journey.caseNumber, journey.features)
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
      .mapWhenDefined(answers.whetherNorthernIreland)(_.submitWhetherNorthernIreland)
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
      .map(_.withDutiesChangeMode(answers.dutiesChangeMode))
      .flatMapWhenDefined(answers.reimbursementMethod)(_.submitReimbursementMethod)
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
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
