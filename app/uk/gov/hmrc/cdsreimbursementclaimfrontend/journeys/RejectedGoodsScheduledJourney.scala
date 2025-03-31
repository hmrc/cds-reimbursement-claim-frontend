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

import cats.Eq
import cats.syntax.eq.*
import com.github.arturopala.validator.Validator
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

import java.time.LocalDate
import java.time.LocalDateTime
import scala.collection.immutable.SortedMap
import java.time.Instant

/** An encapsulated C&E1179 scheduled MRN journey logic. The constructor of this class MUST stay PRIVATE to protected
  * integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *   - [[RejectedGoodsScheduledJourney.Answers]] - keeps record of user answers and acquired documents
  *   - [[RejectedGoodsScheduledJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class RejectedGoodsScheduledJourney private (
  val answers: RejectedGoodsScheduledJourney.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[RejectedGoodsScheduledJourney.Features]
) extends JourneyBase
    with DirectFluentSyntax[RejectedGoodsScheduledJourney]
    with RejectedGoodsJourneyProperties
    with ScheduledVariantProperties
    with HaveInspectionDetails
    with JourneyAnalytics {

  type Type = RejectedGoodsScheduledJourney

  val self: RejectedGoodsScheduledJourney = this

  val validate: Validator.Validate[RejectedGoodsScheduledJourney] =
    RejectedGoodsScheduledJourney.validator

  private def copy(
    newAnswers: RejectedGoodsScheduledJourney.Answers
  ): RejectedGoodsScheduledJourney =
    new RejectedGoodsScheduledJourney(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  def withDutiesChangeMode(enabled: Boolean): RejectedGoodsScheduledJourney =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): RejectedGoodsScheduledJourney =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsScheduledDocumentTypes)

  def removeUnsupportedTaxCodes(): RejectedGoodsScheduledJourney =
    this.copy(answers.copy(displayDeclaration = answers.displayDeclaration.map(_.removeUnsupportedTaxCodes())))

  /** Resets the journey with the new MRN or keep existing journey if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ) =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn)
            if existingMrn === mrn &&
              getLeadDisplayDeclaration.contains(displayDeclaration) =>
          Right(this)
        case _ =>
          if mrn =!= displayDeclaration.getMRN then
            Left(
              "submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
            )
          else
            Right(
              new RejectedGoodsScheduledJourney(
                RejectedGoodsScheduledJourney
                  .Answers(
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    displayDeclaration = Some(displayDeclaration),
                    eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly),
                    nonce = answers.nonce
                  ),
                startTimeSeconds = this.startTimeSeconds,
                features = features
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsScheduledJourney] =
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsScheduledJourney] =
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
        else Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      basisOfClaim match {
        case BasisOfRejectedGoodsClaim.SpecialCircumstances =>
          this.copy(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case _ =>
          this.copy(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              basisOfClaimSpecialCircumstances = None
            )
          )
      }
    }

  def submitBasisOfClaimSpecialCircumstancesDetails(
    basisOfClaimSpecialCircumstancesDetails: String
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      answers.basisOfClaim match {
        case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
          Right(
            this.copy(
              answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstancesDetails))
            )
          )
        case _                                                    =>
          Left("basisOfClaim.not_matching")
      }
    }

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if answers.payeeType.contains(payeeType) then Right(copy(newAnswers = answers.copy(payeeType = Some(payeeType))))
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

  def selectAndReplaceDutyTypeSetForReimbursement(
    dutyTypes: Seq[DutyType]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if dutyTypes.isEmpty then Left("selectAndReplaceDutyTypeSetForReimbursement.emptySelection")
      else {
        val newReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]] =
          SortedMap(
            dutyTypes
              .map(dutyType =>
                dutyType -> getReimbursementClaimsFor(dutyType)
                  .getOrElse(SortedMap.empty[TaxCode, Option[AmountPaidWithCorrect]])
              )*
          )
        Right(this.copy(answers.copy(correctedAmounts = Some(newReimbursementClaims))))
      }
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    dutyType: DutyType,
    taxCodes: Seq[TaxCode]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if !getSelectedDutyTypes.exists(_.contains(dutyType)) then
        Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      else if taxCodes.isEmpty then Left("selectTaxCodeSetForReimbursement.emptySelection")
      else {
        val allTaxCodesMatchDutyType = taxCodes.forall(tc => dutyType.taxCodes.contains(tc))
        if allTaxCodesMatchDutyType then {
          val newReimbursementClaims =
            answers.correctedAmounts
              .map { rc =>
                SortedMap(rc.toSeq.map {
                  case (dt, reimbursementClaims) if dt.repr === dutyType.repr =>
                    dt -> SortedMap(taxCodes.map { tc =>
                      tc -> reimbursementClaims.get(tc).flatten
                    }*)
                  case other                                                  => other
                }*)
              }
          Right(this.copy(answers.copy(correctedAmounts = newReimbursementClaims)))
        } else Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
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
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if dutyType.taxCodes.contains(taxCode) then {
        if isDutySelected(dutyType, taxCode) then {
          val amounts = AmountPaidWithCorrect(paidAmount, correctAmount)
          if amounts.isValid then {
            val newReimbursementClaims =
              answers.correctedAmounts
                .map(rc =>
                  SortedMap(rc.toSeq.map {
                    case (dt, reimbursementClaims) if dt === dutyType =>
                      dt -> SortedMap(reimbursementClaims.toSeq.map {
                        case (tc, _) if tc === taxCode =>
                          tc -> Some(amounts)
                        case other                     => other
                      }*)
                    case other                                        => other
                  }*)
                )
            Right(this.copy(answers.copy(correctedAmounts = newReimbursementClaims)))
          } else Left("submitAmountForReimbursement.invalidReimbursementAmount")
        } else Left("submitAmountForReimbursement.taxCodeNotSelected")
      } else Left("submitAmountForReimbursement.taxCodeNotMatchingDutyType")
    }

  def submitClaimAmount(
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    claimAmount: BigDecimal
  ): Either[String, RejectedGoodsScheduledJourney] =
    submitCorrectAmount(dutyType, taxCode, paidAmount, paidAmount - claimAmount)

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsScheduledJourney] =
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

  def removeBankAccountDetails(): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if needsBanksAccountDetailsSubmission then
        Right(
          this.copy(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def receiveScheduledDocument(
    requestNonce: Nonce,
    uploadedFile: UploadedFile
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        Right(
          this.copy(answers.copy(scheduledDocument = Some(uploadedFile)))
        )
      } else Left("receiveScheduledDocument.invalidNonce")
    }

  def removeScheduledDocument: RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(scheduledDocument = None))
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsScheduledJourney] =
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

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new RejectedGoodsScheduledJourney(
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
    if obj.isInstanceOf[RejectedGoodsScheduledJourney] then {
      val that = obj.asInstanceOf[RejectedGoodsScheduledJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"RejectedGoodsScheduledJourney${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the journey and retrieves the output. */

  def toOutput: Either[Seq[String], RejectedGoodsScheduledJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for
          mrn                    <- getLeadMovementReferenceNumber
          basisOfClaim           <- answers.basisOfClaim
          methodOfDisposal       <- answers.methodOfDisposal
          detailsOfRejectedGoods <- answers.detailsOfRejectedGoods
          inspectionDate         <- answers.inspectionDate
          inspectionAddress      <- answers.inspectionAddress
          scheduledDocument      <- answers.scheduledDocument
          claimantInformation    <- getClaimantInformation
          payeeType              <- getPayeeTypeForOutput(answers.payeeType)
          displayPayeeType       <- answers.payeeType
        yield RejectedGoodsScheduledJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = payeeType,
          displayPayeeType = displayPayeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          methodOfDisposal = methodOfDisposal,
          detailsOfRejectedGoods = detailsOfRejectedGoods,
          inspectionDate = inspectionDate,
          inspectionAddress = inspectionAddress,
          reimbursementClaims = getReimbursementClaims,
          scheduledDocument = EvidenceDocument.from(scheduledDocument),
          supportingEvidences = answers.supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod =
            if isSubsidyOnlyJourney then ReimbursementMethod.Subsidy
            else ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails =
            if isSubsidyOnlyJourney then None
            else answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsScheduledJourney extends JourneyCompanion[RejectedGoodsScheduledJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): RejectedGoodsScheduledJourney =
    new RejectedGoodsScheduledJourney(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]]

  final case class Features(
    shouldBlockSubsidies: Boolean,
    shouldAllowSubsidyOnlyPayments: Boolean
  ) extends SubsidiesFeatures

  // All user answers captured during C&E1179 scheduled MRN journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    payeeType: Option[PayeeType] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    correctedAmounts: Option[CorrectedAmounts] = None,
    inspectionDate: Option[InspectionDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    scheduledDocument: Option[UploadedFile] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    modes: JourneyModes = JourneyModes()
  ) extends RejectedGoodsAnswers
      with ScheduledVariantAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    payeeType: PayeeType,
    displayPayeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    methodOfDisposal: MethodOfDisposal,
    detailsOfRejectedGoods: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    scheduledDocument: EvidenceDocument,
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._
  import JourneyValidationErrors._

  object Checks extends RejectedGoodsJourneyChecks[RejectedGoodsScheduledJourney] {

    val scheduledDocumentHasBeenDefined: Validate[RejectedGoodsScheduledJourney] =
      checkIsDefined(_.answers.scheduledDocument, MISSING_SCHEDULED_DOCUMENT)

  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[RejectedGoodsScheduledJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      containsOnlySupportedTaxCodes,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      scheduledDocumentHasBeenDefined,
      basisOfClaimHasBeenProvided,
      basisOfClaimSpecialCircumstancesHasBeenProvidedIfNeeded,
      detailsOfRejectedGoodsHasBeenProvided,
      inspectionDateHasBeenProvided,
      inspectionAddressHasBeenProvided,
      methodOfDisposalHasBeenProvided,
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

  implicit val format: Format[RejectedGoodsScheduledJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new RejectedGoodsScheduledJourney(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(journey =>
        (journey.answers, journey.startTimeSeconds, journey.caseNumber, journey.submissionDateTime, journey.features)
      )
    )

  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, RejectedGoodsScheduledJourney] =
    empty(answers.userEoriNumber, answers.nonce, features)
      .flatMapWhenDefined(
        answers.movementReferenceNumber.zip(answers.displayDeclaration)
      )(j => { case (mrn: MRN, decl: DisplayDeclaration) =>
        j.submitMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .mapWhenDefined(answers.eoriNumbersVerification.flatMap(_.userXiEori))(_.submitUserXiEori)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber))(_.submitConsigneeEoriNumber)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber))(_.submitDeclarantEoriNumber)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .flatMapWhenDefined(answers.scheduledDocument)(j => d => j.receiveScheduledDocument(j.answers.nonce, d))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .flatMapWhenDefined(answers.basisOfClaimSpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails
      )
      .mapWhenDefined(answers.methodOfDisposal)(_.submitMethodOfDisposal)
      .mapWhenDefined(answers.detailsOfRejectedGoods)(_.submitDetailsOfRejectedGoods)
      .flatMapWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectAndReplaceDutyTypeSetForReimbursement
      )
      .flatMapEachWhenDefined(answers.correctedAmounts)(j => { case (dutyType, reimbursements) =>
        j.selectAndReplaceTaxCodeSetForReimbursement(dutyType, reimbursements.keySet.toSeq)
          .flatMapEachWhenMappingDefined(reimbursements)(j => {
            case (taxCode, AmountPaidWithCorrect(paidAmount, correctAmount)) =>
              j.submitClaimAmount(dutyType, taxCode, paidAmount, paidAmount - correctAmount)
          })
      })
      .map(_.withDutiesChangeMode(answers.dutiesChangeMode))
      .mapWhenDefined(answers.inspectionDate)(_.submitInspectionDate)
      .mapWhenDefined(answers.inspectionAddress)(_.submitInspectionAddress)
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

}
