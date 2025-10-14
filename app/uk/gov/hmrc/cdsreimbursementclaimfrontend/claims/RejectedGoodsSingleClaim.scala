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

import cats.Eq
import cats.syntax.eq.*
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime

/** An encapsulated C&E1179 single MRN claim logic. The constructor of this class MUST stay PRIVATE to protected
  * integrity of the claim.
  *
  * The claim uses two nested case classes:
  *
  *   - [[RejectedGoodsSingleClaim.Answers]] - keeps record of user answers and acquired documents
  *   - [[RejectedGoodsSingleClaim.Output]] - final output of the claim to be sent to backend processing
  */
final class RejectedGoodsSingleClaim private (
  val answers: RejectedGoodsSingleClaim.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[RejectedGoodsSingleClaim.Features]
) extends ClaimBase
    with DirectFluentSyntax[RejectedGoodsSingleClaim]
    with RejectedGoodsClaimProperties
    with SingleVariantProperties
    with HaveInspectionDetails
    with ClaimAnalytics {

  type Type = RejectedGoodsSingleClaim

  val self: RejectedGoodsSingleClaim = this

  val validate: Validator.Validate[RejectedGoodsSingleClaim] =
    RejectedGoodsSingleClaim.validator

  private def copy(
    newAnswers: RejectedGoodsSingleClaim.Answers
  ): RejectedGoodsSingleClaim =
    new RejectedGoodsSingleClaim(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  override def filterAvailableDuties(duties: Seq[(TaxCode, Boolean)]): Seq[(TaxCode, Boolean)] =
    duties

  def withDutiesChangeMode(enabled: Boolean): RejectedGoodsSingleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): RejectedGoodsSingleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsSingleDocumentTypes)

  def removeUnsupportedTaxCodes(): RejectedGoodsSingleClaim =
    this.copy(answers.copy(importDeclaration = answers.importDeclaration.map(_.removeUnsupportedTaxCodes())))

  /** Resets the claim with the new MRN or keep existing claim if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ) =
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
              new RejectedGoodsSingleClaim(
                RejectedGoodsSingleClaim
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

  def submitUserXiEori(userXiEori: UserXiEori): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsSingleClaim] =
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsSingleClaim] =
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

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsSingleClaim =
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
  ): Either[String, RejectedGoodsSingleClaim] =
    whileClaimIsAmendable {
      answers.basisOfClaim match {
        case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
          Right(
            this.copy(
              answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstancesDetails))
            )
          )
        case _                                                    => Left("basisOfClaim.not_matching")
      }
    }

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, RejectedGoodsSingleClaim] =
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
  ): Either[String, RejectedGoodsSingleClaim] =
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

            case _ =>
              Left("submitCorrectAmount.invalidAmount")
          }
      }
    }

  def submitClaimAmount(
    taxCode: TaxCode,
    claimAmount: BigDecimal
  ): Either[String, RejectedGoodsSingleClaim] =
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

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, RejectedGoodsSingleClaim] =
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsSingleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountDetails =
            Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
          )
        )
      )
    }

  def removeBankAccountDetails(): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsSingleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitReimbursementMethod(
    reimbursementMethod: ReimbursementMethod
  ): Either[String, RejectedGoodsSingleClaim] =
    whileClaimIsAmendable {

      if reimbursementMethod === ReimbursementMethod.CurrentMonthAdjustment then {
        if isAllSelectedDutiesAreCMAEligible then {
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod)
              )
            )
          )
        } else Left("submitReimbursementMethod.notCMAEligible")
      } else
        Right(
          this.copy(
            answers.copy(
              reimbursementMethod = Some(reimbursementMethod)
            )
          )
        )

    }

  def resetReimbursementMethod(): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(
          reimbursementMethod = None,
          bankAccountType = None,
          bankAccountDetails = None
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsSingleClaim] =
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

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsSingleClaim =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeClaimWith(caseNumber: String): Either[String, RejectedGoodsSingleClaim] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new RejectedGoodsSingleClaim(
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
    if obj.isInstanceOf[RejectedGoodsSingleClaim] then {
      val that = obj.asInstanceOf[RejectedGoodsSingleClaim]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"RejectedGoodsSingleClaim${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the claim and retrieves the output. */

  def toOutput: Either[Seq[String], RejectedGoodsSingleClaim.Output] =
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
          supportingEvidences     = answers.supportingEvidences
          claimantInformation    <- getClaimantInformation
          payeeType              <- getPayeeTypeForOutput(answers.payeeType)
          displayPayeeType       <- answers.payeeType
        yield RejectedGoodsSingleClaim.Output(
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
          reimbursements = getReimbursements,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod = answers.reimbursementMethod.getOrElse(ReimbursementMethod.BankAccountTransfer),
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsSingleClaim extends ClaimCompanion[RejectedGoodsSingleClaim] {

  /** A starting point to build new instance of the claim. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): RejectedGoodsSingleClaim =
    new RejectedGoodsSingleClaim(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = Map[TaxCode, Option[BigDecimal]]

  final case class Features()

  // All user answers captured during C&E1179 single MRN claim
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    importDeclaration: Option[ImportDeclaration] = None,
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
    reimbursementMethod: Option[ReimbursementMethod] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    modes: ClaimModes = ClaimModes()
  ) extends RejectedGoodsAnswers
      with SingleVariantAnswers

  // Final minimal output of the claim we want to pass to the backend.
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
    reimbursements: Seq[Reimbursement],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  ) extends WafErrorMitigation[Output] {

    override def excludeFreeTextInputs() =
      (
        Seq(("additional_details", detailsOfRejectedGoods))
          ++ basisOfClaimSpecialCircumstances.map(v => Seq(("special_circumstances", v))).getOrElse(Seq.empty),
        this.copy(
          detailsOfRejectedGoods = additionalDetailsReplacementText,
          basisOfClaimSpecialCircumstances =
            basisOfClaimSpecialCircumstances.map(_ => specialCircumstancesReplacementText)
        )
      )
  }

  import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator._
  import ClaimValidationErrors._

  object Checks extends RejectedGoodsClaimChecks[RejectedGoodsSingleClaim] {

    val reimbursementMethodHasBeenProvidedIfNeeded: Validate[RejectedGoodsSingleClaim] =
      all(
        whenTrue(
          j => j.isAllSelectedDutiesAreCMAEligible,
          checkIsDefined(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_MUST_BE_DEFINED
          )
        )
      )
  }

  import Checks._

  /** Validate if all required answers has been provided and the claim is ready to produce output. */
  override implicit val validator: Validate[RejectedGoodsSingleClaim] =
    all(
      hasMRNAndImportDeclaration,
      containsOnlySupportedTaxCodes,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      basisOfClaimSpecialCircumstancesHasBeenProvidedIfNeeded,
      detailsOfRejectedGoodsHasBeenProvided,
      inspectionDateHasBeenProvided,
      inspectionAddressHasBeenProvided,
      methodOfDisposalHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      reimbursementMethodHasBeenProvidedIfNeeded,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided,
      declarationsHasNoSubsidyPayments
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

  implicit val format: Format[RejectedGoodsSingleClaim] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new RejectedGoodsSingleClaim(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(claim =>
        (claim.answers, claim.startTimeSeconds, claim.caseNumber, claim.submissionDateTime, claim.features)
      )
    )

  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, RejectedGoodsSingleClaim] =
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
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .flatMapWhenDefined(answers.basisOfClaimSpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails
      )
      .mapWhenDefined(answers.methodOfDisposal)(_.submitMethodOfDisposal)
      .mapWhenDefined(answers.detailsOfRejectedGoods)(_.submitDetailsOfRejectedGoods)
      .flatMapWhenDefined(answers.correctedAmounts.map(_.keySet.toSeq))(
        _.selectAndReplaceTaxCodeSetForReimbursement
      )
      .flatMapEachWhenDefinedAndMappingDefined(answers.correctedAmounts)(_.submitCorrectAmount)
      .mapWhenDefined(answers.inspectionDate)(_.submitInspectionDate)
      .mapWhenDefined(answers.inspectionAddress)(_.submitInspectionAddress)
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
      .flatMapWhenDefined(answers.reimbursementMethod)(_.submitReimbursementMethod)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

  /** This method MUST BE used only to test the validation correctness of the invalid answer states. */
  def unsafeModifyAnswers(
    claim: RejectedGoodsSingleClaim,
    f: RejectedGoodsSingleClaim.Answers => RejectedGoodsSingleClaim.Answers
  ): RejectedGoodsSingleClaim =
    RejectedGoodsSingleClaim(
      answers = f(claim.answers),
      startTimeSeconds = claim.startTimeSeconds,
      features = claim.features
    )

}
