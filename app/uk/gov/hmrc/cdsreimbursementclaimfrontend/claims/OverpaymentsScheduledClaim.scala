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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator

import java.time.Instant
import java.time.LocalDateTime
import scala.collection.immutable.SortedMap

/** An encapsulated C285 scheduled MRN claim logic. The constructor of this class MUST stay PRIVATE to protected
  * integrity of the claim.
  *
  * The claim uses two nested case classes:
  *
  *   - [[OverpaymentsScheduledClaim.Answers]] - keeps record of user answers and acquired documents
  *   - [[OverpaymentsScheduledClaim.Output]] - final output of the claim to be sent to backend processing
  */
final class OverpaymentsScheduledClaim private (
  val answers: OverpaymentsScheduledClaim.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[OverpaymentsScheduledClaim.Features]
) extends ClaimBase
    with DirectFluentSyntax[OverpaymentsScheduledClaim]
    with OverpaymentsClaimProperties
    with ScheduledVariantProperties
    with ClaimAnalytics {

  type Type = OverpaymentsScheduledClaim

  val self: OverpaymentsScheduledClaim = this

  val validate: Validator.Validate[OverpaymentsScheduledClaim] =
    OverpaymentsScheduledClaim.validator

  private def copy(
    newAnswers: OverpaymentsScheduledClaim.Answers
  ): OverpaymentsScheduledClaim =
    new OverpaymentsScheduledClaim(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsScheduledDocumentTypes)

  def removeUnsupportedTaxCodes(): OverpaymentsScheduledClaim =
    this.copy(answers.copy(importDeclaration = answers.importDeclaration.map(_.removeUnsupportedTaxCodes())))

  /** Resets the claim with the new MRN or keep existing claim if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ): Either[String, OverpaymentsScheduledClaim] =
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
              new OverpaymentsScheduledClaim(
                OverpaymentsScheduledClaim
                  .Answers(
                    nonce = answers.nonce,
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    importDeclaration = Some(importDeclaration),
                    eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly)
                  ),
                startTimeSeconds = this.startTimeSeconds,
                features = features
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsScheduledClaim] =
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, OverpaymentsScheduledClaim] =
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

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(basisOfClaim = Some(basisOfClaim)))
    }

  def submitNewEori(eori: Eori): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newEori = Some(eori))
      )
    }

  def submitNewDan(dan: Dan): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newDan = Some(dan))
      )
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }
  def selectAndReplaceDutyTypeSetForReimbursement(
    dutyTypes: Seq[DutyType]
  ): Either[String, OverpaymentsScheduledClaim] =
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
        Right(
          this.copy(
            answers.copy(
              correctedAmounts = Some(newReimbursementClaims),
              exciseCategories = if dutyTypes.contains(DutyType.Excise) then answers.exciseCategories else None
            )
          )
        )
      }
    }

  def selectAndReplaceTaxCodeSetForDutyType(
    dutyType: DutyType,
    taxCodes: Seq[TaxCode]
  ): Either[String, OverpaymentsScheduledClaim] =
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

  def selectAndReplaceExciseCodeCategories(
    exciseCategories: Seq[ExciseCategory]
  ): Either[String, OverpaymentsScheduledClaim] =
    whileClaimIsAmendable {
      if !getSelectedDutyTypes.exists(_.contains(DutyType.Excise))
      then Left("selectAndReplaceExciseCodeCategories.exciseDutyTypeNotSelected")
      else if exciseCategories.isEmpty
      then Left("selectAndReplaceExciseCodeCategories.emptySelection")
      else
        val exciseCategoriesSet = exciseCategories.toSet
        Right(
          this.copy(
            answers.copy(
              exciseCategories = Some(exciseCategories),
              // remove eventually tax codes belonging to the unchecked excise category
              correctedAmounts = answers.correctedAmounts.map(_.map {
                case (DutyType.Excise, claims) =>
                  (
                    DutyType.Excise,
                    claims.filter((tc, _) => exciseCategoriesSet.contains(ExciseCategory.categoryOf(tc)))
                  )
                case other                     => other
              })
            )
          )
        )
    }

  def selectAndReplaceTaxCodeSetForExciseCategory(
    exciseCategory: ExciseCategory,
    taxCodes: Seq[TaxCode]
  ): Either[String, OverpaymentsScheduledClaim] =
    whileClaimIsAmendable {
      if !getSelectedDutyTypes.exists(_.contains(DutyType.Excise)) then
        Left("selectAndReplaceTaxCodeSetForExciseCategory.dutyTypeNotSelectedBefore")
      else if taxCodes.isEmpty then Left("selectAndReplaceTaxCodeSetForExciseCategory.emptySelection")
      else {
        val allTaxCodesMatchDutyType = taxCodes.forall(tc =>
          DutyType.Excise.taxCodes.contains(tc)
            && ExciseCategory.categoryOf(tc) === exciseCategory
        )
        if allTaxCodesMatchDutyType then {
          val newReimbursementClaims =
            answers.correctedAmounts
              .map { rc =>
                SortedMap(rc.toSeq.map {
                  case (dt, reimbursementClaims) if dt === DutyType.Excise =>
                    dt -> (reimbursementClaims.filterNot((tc, _) =>
                      ExciseCategory.categoryOf(tc) == exciseCategory
                    ) ++ SortedMap(taxCodes.map { tc =>
                      tc -> reimbursementClaims.get(tc).flatten
                    }*))
                  case other                                               => other
                }*)
              }
          Right(this.copy(answers.copy(correctedAmounts = newReimbursementClaims)))
        } else Left("selectAndReplaceTaxCodeSetForExciseCategory.someTaxCodesDoesNotMatchDutyType")
      }
    }

  def getAvailableClaimTypes: Set[BasisOfOverpaymentClaim] =
    BasisOfOverpaymentClaim
      .excludeNorthernIrelandClaims(
        hasDuplicateEntryClaim = false,
        answers.importDeclaration,
        isOtherEnabled = features.exists(_.shouldAllowOtherBasisOfClaim)
      )

  def isDutySelected(dutyType: DutyType, taxCode: TaxCode): Boolean =
    answers.correctedAmounts
      .exists(_.exists { case (dt, tca) => dt === dutyType && tca.exists(_._1 === taxCode) })

  def submitCorrectAmount(
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    correctAmount: BigDecimal
  ): Either[String, OverpaymentsScheduledClaim] =
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
  ): Either[String, OverpaymentsScheduledClaim] =
    submitCorrectAmount(dutyType, taxCode, paidAmount, paidAmount - claimAmount)

  def submitPayeeType(payeeType: PayeeType): Either[String, OverpaymentsScheduledClaim] =
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsScheduledClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountDetails =
            Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
          )
        )
      )
    }

  def removeBankAccountDetails(): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsScheduledClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsScheduledClaim] =
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

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeClaimWith(caseNumber: String): Either[String, OverpaymentsScheduledClaim] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new OverpaymentsScheduledClaim(
                answers = this.answers,
                startTimeSeconds = this.startTimeSeconds,
                caseNumber = Some(caseNumber),
                submissionDateTime = Some(LocalDateTime.now()),
                features = features
              )
            )
        )
    }

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsScheduledClaim =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): OverpaymentsScheduledClaim =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  def receiveScheduledDocument(
    requestNonce: Nonce,
    uploadedFile: UploadedFile
  ): Either[String, OverpaymentsScheduledClaim] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        Right(
          this.copy(answers.copy(scheduledDocument = Some(uploadedFile)))
        )
      } else Left("receiveScheduledDocument.invalidNonce")
    }

  def removeScheduledDocument: OverpaymentsScheduledClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(scheduledDocument = None))
    }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[OverpaymentsScheduledClaim] then {
      val that = obj.asInstanceOf[OverpaymentsScheduledClaim]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"OverpaymentsScheduledClaim${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the claim and retrieves the output. */

  def toOutput: Either[Seq[String], OverpaymentsScheduledClaim.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for
          mrn                 <- getLeadMovementReferenceNumber
          basisOfClaim        <- answers.basisOfClaim
          additionalDetails   <- answers.additionalDetails
          scheduledDocument   <- answers.scheduledDocument
          claimantInformation <- getClaimantInformation
          payeeType           <- getPayeeTypeForOutput(answers.payeeType)
          displayPayeeType    <- answers.payeeType
          newEoriAndDan        = (basisOfClaim, answers.newEori, answers.newDan) match {
                                   case (IncorrectEoriAndDan, Some(newEori), Some(newDan)) =>
                                     Some(NewEoriAndDan(newEori, newDan.value))
                                   case _                                                  => None
                                 }
        yield OverpaymentsScheduledClaim.Output(
          movementReferenceNumber = mrn,
          scheduledDocument = EvidenceDocument.from(scheduledDocument),
          claimantType = getClaimantType,
          payeeType = payeeType,
          displayPayeeType = displayPayeeType,
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

object OverpaymentsScheduledClaim extends ClaimCompanion[OverpaymentsScheduledClaim] {

  /** A starting point to build new instance of the claim. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): OverpaymentsScheduledClaim =
    new OverpaymentsScheduledClaim(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]]

  final case class Features(
    shouldAllowOtherBasisOfClaim: Boolean = true
  )

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    scheduledDocument: Option[UploadedFile] = None,
    importDeclaration: Option[ImportDeclaration] = None,
    payeeType: Option[PayeeType] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfOverpaymentClaim] = None,
    additionalDetails: Option[String] = None,
    correctedAmounts: Option[CorrectedAmounts] = None,
    exciseCategories: Option[Seq[ExciseCategory]] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    newEori: Option[Eori] = None,
    newDan: Option[Dan] = None,
    modes: ClaimModes = ClaimModes()
  ) extends OverpaymentsAnswers
      with ScheduledVariantAnswers

  final case class Output(
    movementReferenceNumber: MRN,
    scheduledDocument: EvidenceDocument,
    claimantType: ClaimantType,
    payeeType: PayeeType,
    displayPayeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    reimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]],
    reimbursementMethod: ReimbursementMethod,
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

  import ClaimValidationErrors._
  import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator._

  object Checks extends OverpaymentsClaimChecks[OverpaymentsScheduledClaim] {

    val scheduledDocumentHasBeenDefined: Validate[OverpaymentsScheduledClaim] =
      checkIsDefined(_.answers.scheduledDocument, MISSING_SCHEDULED_DOCUMENT)
  }

  import Checks._

  /** Validate if all required answers has been provided and the claim is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsScheduledClaim] =
    all(
      hasMRNAndImportDeclaration,
      containsOnlySupportedTaxCodes,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      scheduledDocumentHasBeenDefined,
      basisOfClaimHasBeenProvided,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided,
      declarationsHasNoSubsidyPayments,
      payeeTypeIsDefined,
      newEoriAndDanProvidedIfNeeded
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

  implicit val format: Format[OverpaymentsScheduledClaim] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new OverpaymentsScheduledClaim(_, _, _, _, _)),
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
  ): Either[String, OverpaymentsScheduledClaim] =
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
      .flatMapWhenDefined(answers.scheduledDocument)(j => d => j.receiveScheduledDocument(j.answers.nonce, d))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectAndReplaceDutyTypeSetForReimbursement
      )
      .flatMapWhenDefined(answers.exciseCategories)(_.selectAndReplaceExciseCodeCategories)
      .flatMapEachWhenDefined(answers.correctedAmounts)(j => { case (dutyType, reimbursements) =>
        j.selectAndReplaceTaxCodeSetForDutyType(dutyType, reimbursements.keySet.toSeq)
          .flatMapEachWhenMappingDefined(reimbursements)(j => {
            case (taxCode, AmountPaidWithCorrect(paidAmount, correctAmount)) =>
              j.submitClaimAmount(dutyType, taxCode, paidAmount, paidAmount - correctAmount)
          })
      })
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
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

  /** This method MUST BE used only to test the validation correctness of the invalid answer states.. */
  def unsafeModifyAnswers(
    claim: OverpaymentsScheduledClaim,
    f: OverpaymentsScheduledClaim.Answers => OverpaymentsScheduledClaim.Answers
  ): OverpaymentsScheduledClaim =
    OverpaymentsScheduledClaim(
      answers = f(claim.answers),
      startTimeSeconds = claim.startTimeSeconds,
      features = claim.features
    )

}
