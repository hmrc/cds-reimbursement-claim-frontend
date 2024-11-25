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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

import java.time.LocalDateTime
import scala.collection.immutable.SortedMap

/** An encapsulated C285 scheduled MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[OverpaymentsScheduledJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[OverpaymentsScheduledJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class OverpaymentsScheduledJourney private (
  val answers: OverpaymentsScheduledJourney.Answers,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[OverpaymentsScheduledJourney.Features]
) extends JourneyBase
    with DirectFluentSyntax[OverpaymentsScheduledJourney]
    with OverpaymentsJourneyProperties
    with ScheduledVariantProperties
    with JourneyAnalytics {

  type Type = OverpaymentsScheduledJourney

  val self: OverpaymentsScheduledJourney = this

  val validate: Validator.Validate[OverpaymentsScheduledJourney] =
    OverpaymentsScheduledJourney.validator

  private def copy(
    newAnswers: OverpaymentsScheduledJourney.Answers
  ): OverpaymentsScheduledJourney =
    new OverpaymentsScheduledJourney(newAnswers, caseNumber, submissionDateTime, features)

  def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsScheduledDocumentTypes)

  def getAvailableClaimTypes(showDanOption: Boolean = false): Set[BasisOfOverpaymentClaim] =
    BasisOfOverpaymentClaim
      .excludeNorthernIrelandClaims(
        hasDuplicateEntryClaim = false,
        getLeadDisplayDeclaration,
        hasDanOption = false
      )

  def removeUnsupportedTaxCodes(): OverpaymentsScheduledJourney =
    this.copy(answers.copy(displayDeclaration = answers.displayDeclaration.map(_.removeUnsupportedTaxCodes())))

  /** Resets the journey with the new MRN
    * or keep existing journey if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, OverpaymentsScheduledJourney] =
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
              new OverpaymentsScheduledJourney(
                OverpaymentsScheduledJourney
                  .Answers(
                    nonce = answers.nonce,
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    displayDeclaration = Some(displayDeclaration),
                    eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly)
                  ),
                features = features
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsScheduledJourney] =
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, OverpaymentsScheduledJourney] =
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

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(basisOfClaim = Some(basisOfClaim)))
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsScheduledJourney                 =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }
  def selectAndReplaceDutyTypeSetForReimbursement(
    dutyTypes: Seq[DutyType]
  ): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (dutyTypes.isEmpty)
        Left("selectAndReplaceDutyTypeSetForReimbursement.emptySelection")
      else {
        val newReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]] =
          SortedMap(
            dutyTypes
              .map(dutyType =>
                (dutyType -> getReimbursementClaimsFor(dutyType)
                  .getOrElse(SortedMap.empty[TaxCode, Option[AmountPaidWithCorrect]]))
              ): _*
          )
        Right(this.copy(answers.copy(correctedAmounts = Some(newReimbursementClaims))))
      }
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    dutyType: DutyType,
    taxCodes: Seq[TaxCode]
  ): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (!getSelectedDutyTypes.exists(_.contains(dutyType)))
        Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      else if (taxCodes.isEmpty)
        Left("selectTaxCodeSetForReimbursement.emptySelection")
      else {
        val allTaxCodesMatchDutyType = taxCodes.forall(tc => dutyType.taxCodes.contains(tc))
        if (allTaxCodesMatchDutyType) {
          val newReimbursementClaims =
            answers.correctedAmounts
              .map { rc =>
                SortedMap(rc.toSeq.map {
                  case (dt, reimbursementClaims) if dt.repr === dutyType.repr =>
                    dt -> SortedMap(taxCodes.map { tc =>
                      tc -> reimbursementClaims.get(tc).flatten
                    }: _*)
                  case other                                                  => other
                }: _*)
              }
          Right(this.copy(answers.copy(correctedAmounts = newReimbursementClaims)))
        } else
          Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

  def isDutySelected(dutyType: DutyType, taxCode: TaxCode): Boolean =
    answers.correctedAmounts
      .exists(_.exists { case (dt, tca) => dt === dutyType && tca.exists(_._1 === taxCode) })

  def submitCorrectAmount(
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    correctAmount: BigDecimal
  ): Either[String, OverpaymentsScheduledJourney]                   =
    whileClaimIsAmendable {
      if (dutyType.taxCodes.contains(taxCode)) {
        if (isDutySelected(dutyType, taxCode)) {
          val amounts = AmountPaidWithCorrect(paidAmount, correctAmount)
          if (amounts.isValid) {
            val newReimbursementClaims =
              answers.correctedAmounts
                .map(rc =>
                  SortedMap(rc.toSeq.map {
                    case (dt, reimbursementClaims) if dt === dutyType =>
                      dt -> SortedMap(reimbursementClaims.toSeq.map {
                        case (tc, _) if tc === taxCode =>
                          tc -> Some(amounts)
                        case other                     => other
                      }: _*)
                    case other                                        => other
                  }: _*)
                )
            Right(this.copy(answers.copy(correctedAmounts = newReimbursementClaims)))
          } else
            Left("submitAmountForReimbursement.invalidReimbursementAmount")
        } else
          Left("submitAmountForReimbursement.taxCodeNotSelected")
      } else
        Left("submitAmountForReimbursement.taxCodeNotMatchingDutyType")
    }

  def submitClaimAmount(
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    claimAmount: BigDecimal
  ): Either[String, OverpaymentsScheduledJourney] =
    submitCorrectAmount(dutyType, taxCode, paidAmount, paidAmount - claimAmount)

  def submitPayeeType(payeeType: PayeeType): Either[String, OverpaymentsScheduledJourney] =
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountDetails =
            Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
          )
        )
      )
    }

  def removeBankAccountDetails(): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsScheduledJourney] =
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

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new OverpaymentsScheduledJourney(
                answers = this.answers,
                caseNumber = Some(caseNumber),
                submissionDateTime = Some(LocalDateTime.now()),
                features = features
              )
            )
        )
    }

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsScheduledJourney =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): OverpaymentsScheduledJourney =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveScheduledDocument(
    requestNonce: Nonce,
    uploadedFile: UploadedFile
  ): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        Right(
          this.copy(answers.copy(scheduledDocument = Some(uploadedFile)))
        )
      } else Left("receiveScheduledDocument.invalidNonce")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def removeScheduledDocument: OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(scheduledDocument = None))
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[OverpaymentsScheduledJourney]) {
      val that = obj.asInstanceOf[OverpaymentsScheduledJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"OverpaymentsScheduledJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[Seq[String], OverpaymentsScheduledJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for {
          mrn                 <- getLeadMovementReferenceNumber
          basisOfClaim        <- answers.basisOfClaim
          additionalDetails   <- answers.additionalDetails
          scheduledDocument   <- answers.scheduledDocument
          claimantInformation <- getClaimantInformation
          payeeType           <- answers.payeeType
          newEoriAndDan        = (basisOfClaim, answers.newEori, answers.newDan) match {
                                   case (IncorrectEoriAndDan, Some(newEori), Some(newDan)) =>
                                     Some(NewEoriAndDan(newEori, newDan.value))
                                   case _                                                  => None
                                 }
        } yield OverpaymentsScheduledJourney.Output(
          movementReferenceNumber = mrn,
          scheduledDocument = EvidenceDocument.from(scheduledDocument),
          claimantType = getClaimantType,
          payeeType = payeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          additionalDetails = additionalDetails,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences = answers.supportingEvidences.map(EvidenceDocument.from),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails,
          newEoriAndDan = newEoriAndDan
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object OverpaymentsScheduledJourney extends JourneyCompanion[OverpaymentsScheduledJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): OverpaymentsScheduledJourney =
    new OverpaymentsScheduledJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce), features = features)

  type CorrectedAmounts = SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]]

  final case class Features(
    shouldBlockSubsidies: Boolean,
    shouldAllowSubsidyOnlyPayments: Boolean
  ) extends SubsidiesFeatures

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    scheduledDocument: Option[UploadedFile] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    payeeType: Option[PayeeType] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfOverpaymentClaim] = None,
    additionalDetails: Option[String] = None,
    correctedAmounts: Option[CorrectedAmounts] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    newEori: Option[Eori] = None,
    newDan: Option[Dan] = None,
    modes: JourneyModes = JourneyModes()
  ) extends OverpaymentsAnswers
      with ScheduledVariantAnswers

  final case class Output(
    movementReferenceNumber: MRN,
    scheduledDocument: EvidenceDocument,
    claimantType: ClaimantType,
    payeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    reimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument],
    newEoriAndDan: Option[NewEoriAndDan]
  )

  import JourneyValidationErrors._
  import com.github.arturopala.validator.Validator._

  object Checks extends OverpaymentsJourneyChecks[OverpaymentsScheduledJourney] {

    val scheduledDocumentHasBeenDefined: Validate[OverpaymentsScheduledJourney] =
      checkIsDefined(_.answers.scheduledDocument, MISSING_SCHEDULED_DOCUMENT)
  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsScheduledJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      containsOnlySupportedTaxCodes,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      scheduledDocumentHasBeenDefined,
      basisOfClaimHasBeenProvided,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
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

  implicit val format: Format[OverpaymentsScheduledJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new OverpaymentsScheduledJourney(_, _, _, _)),
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
  ): Either[String, OverpaymentsScheduledJourney] =
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
      .flatMapWhenDefined(answers.scheduledDocument)(j => d => j.receiveScheduledDocument(j.answers.nonce, d))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectAndReplaceDutyTypeSetForReimbursement
      )
      .flatMapEachWhenDefined(answers.correctedAmounts)(j => { case (dutyType, reimbursements) =>
        j.selectAndReplaceTaxCodeSetForReimbursement(dutyType, reimbursements.keySet.toSeq)
          .flatMapEachWhenMappingDefined(reimbursements)(j => {
            case (taxCode, AmountPaidWithCorrect(paidAmount, correctAmount)) =>
              j.submitCorrectAmount(dutyType, taxCode, paidAmount, correctAmount)
          })
      })
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType _)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

}
