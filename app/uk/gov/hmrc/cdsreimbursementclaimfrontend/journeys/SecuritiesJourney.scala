/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.syntax.eq._
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DEC91Response
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import com.github.arturopala.validator.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat
import scala.collection.immutable.SortedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentImplicits
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.TaxDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

final class SecuritiesJourney private (
  val answers: SecuritiesJourney.Answers,
  val caseNumber: Option[String] = None
) extends Claim[SecuritiesJourney]
    with CommonJourneyProperties
    with FluentSyntax[SecuritiesJourney] {

  import SecuritiesJourney.Answers
  import SecuritiesJourney.Checks._
  import SecuritiesJourney.SecuritiesReclaims

  override def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  override def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def getSecurityDepositIds: Seq[String] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityDepositIds)
      .getOrElse(Seq.empty)

  def isValidSecurityDepositId(securityDepositId: String): Boolean =
    getLeadDisplayDeclaration
      .exists(_.isValidSecurityDepositId(securityDepositId))

  def getSecurityDetailsFor(securityDepositId: String): Option[SecurityDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityDetailsFor(securityDepositId))

  def getSecurityTaxDetailsFor(securityDepositId: String, taxCode: TaxCode): Option[TaxDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityTaxDetailsFor(securityDepositId, taxCode))

  def getSecurityDepositAmountFor(securityDepositId: String, taxCode: TaxCode): Option[BigDecimal] =
    getSecurityTaxDetailsFor(securityDepositId, taxCode).map(_.amount).map(BigDecimal.apply)

  def getSecurityTaxCodesFor(securityDepositId: String): Seq[TaxCode] =
    getLeadDisplayDeclaration
      .map(_.getSecurityTaxCodesFor(securityDepositId))
      .getOrElse(Seq.empty)

  def getSelectedDepositIds: Seq[String] =
    answers.securitiesReclaims.map(_.keys.toSeq).getOrElse(Seq.empty)

  def isSelectedDepositId(securityDepositId: String): Boolean =
    answers.securitiesReclaims.exists(_.contains(securityDepositId))

  def getSelectedDutiesFor(securityDepositId: String): Option[Seq[TaxCode]] =
    answers.securitiesReclaims.flatMap(_.get(securityDepositId).map(_.keys.toSeq))

  def getAllSelectedDuties: Seq[(String, TaxCode)] =
    answers.securitiesReclaims
      .map(_.toSeq.flatMap { case (sid, reclaims) =>
        reclaims.keys.map(tc => (sid, tc))
      })
      .getOrElse(Seq.empty)

  def getReclaimAmountFor(securityDepositId: String, taxCode: TaxCode): Option[BigDecimal] =
    answers.securitiesReclaims
      .flatMap(_.get(securityDepositId))
      .flatMap(_.get(taxCode))
      .flatten

  def getTotalReclaimAmount: BigDecimal =
    answers.securitiesReclaims
      .map(_.map(_._2.map(_._2.getOrElse(ZERO)).sum).sum)
      .getOrElse(ZERO)

  def getSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]] =
    answers.securitiesReclaims
      .map(_.mapValues(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) }))
      .getOrElse(SortedMap.empty)

  def requiresExportDeclaration: Boolean =
    ReasonForSecurity.requiresExportDeclaration
      .exists(answers.reasonForSecurity.contains(_))

  def goodsHasBeenAlreadyExported: Boolean =
    answers.exportDeclaration.exists(_.goodsHasBeenAlreadyExported)

  def hasCompleteSecuritiesReclaims: Boolean =
    answers.securitiesReclaims.nonEmpty &&
      answers.securitiesReclaims.forall(m =>
        m.nonEmpty && m.forall(_._2.nonEmpty) && m.forall(_._2.forall(_._2.isDefined))
      )

  def isAllSelectedDutiesAreGuaranteeEligible: Boolean =
    getSelectedDepositIds
      .map(getSecurityDetailsFor)
      .collect { case Some(s) => s }
      .forall(_.isGuaranteeEligible)

  def needsBanksAccountDetailsSubmission: Boolean =
    !isAllSelectedDutiesAreGuaranteeEligible

  def getReasonForSecurity: Option[ReasonForSecurity] =
    answers.reasonForSecurity

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
          new SecuritiesJourney(
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
          new SecuritiesJourney(
            Answers(
              userEoriNumber = answers.userEoriNumber,
              movementReferenceNumber = answers.movementReferenceNumber,
              nonce = answers.nonce,
              reasonForSecurity = Some(reasonForSecurity),
              displayDeclaration = Some(displayDeclaration)
            )
          )
        )
    }

  def submitClaimDuplicateCheckStatus(
    similarClaimExistAlreadyInCDFPay: Boolean
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMovementReferenceNumber && hasReasonForSecurity) {
      Right(
        new SecuritiesJourney(
          answers.copy(
            similarClaimExistAlreadyInCDFPay = Some(similarClaimExistAlreadyInCDFPay)
          )
        )
      )
    }

  def submitExportMovementReferenceNumberAndDeclaration(
    exportMrn: MRN,
    exportDeclaration: DEC91Response
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(hasMRNAndDisplayDeclarationAndRfS && thereIsNoSimilarClaimInCDFPay) {
      if (requiresExportDeclaration)
        Right(
          new SecuritiesJourney(
            answers.copy(
              exportMovementReferenceNumber = Some(exportMrn),
              exportDeclaration = Some(exportDeclaration)
            )
          )
        )
      else
        Left("submitExportMovementReferenceNumberAndDeclaration.exportDeclarationNotRequired")
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
          new SecuritiesJourney(
            answers.copy(
              securitiesReclaims = answers.securitiesReclaims
                .map(m =>
                  (emptySecuritiesReclaims ++ m)
                    .filterKeys(securityDepositIds.contains(_))
                )
                .orElse(Some(emptySecuritiesReclaims))
            )
          )
        )
      }
    }

  def selectSecurityDepositId(securityDepositId: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (!isValidSecurityDepositId(securityDepositId))
        Left("selectSecurityDepositIds.invalidSecurityDepositId")
      else {
        if (answers.securitiesReclaims.contains(securityDepositId))
          Right(this)
        else {
          val emptySecuritiesReclaim =
            SortedMap(securityDepositId -> SortedMap.empty[TaxCode, Option[BigDecimal]])
          Right(
            new SecuritiesJourney(
              answers.copy(
                securitiesReclaims = answers.securitiesReclaims
                  .map(_ ++ emptySecuritiesReclaim)
                  .orElse(Some(emptySecuritiesReclaim))
              )
            )
          )
        }
      }
    }

  def selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(
    securityDepositId: String,
    taxCodes: Seq[TaxCode]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (!isValidSecurityDepositId(securityDepositId))
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      else if (!isSelectedDepositId(securityDepositId))
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.securityDepositIdNotSelected")
      else if (taxCodes.isEmpty)
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      else if (!getSecurityTaxCodesFor(securityDepositId).containsEachItemOf(taxCodes))
        Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      else {
        val existingReclaims: SecuritiesReclaims =
          answers.securitiesReclaims
            .flatMap(_.get(securityDepositId))
            .getOrElse(SortedMap.empty)
        val refinedReclaims: SecuritiesReclaims  =
          SortedMap(taxCodes.map(taxCode => taxCode -> existingReclaims.getOrElse(taxCode, None)): _*)
        Right(
          new SecuritiesJourney(
            answers.copy(
              securitiesReclaims = answers.securitiesReclaims
                .map(_ + (securityDepositId -> refinedReclaims))
            )
          )
        )
      }
    }

  def isValidReclaimAmount(reclaimAmount: BigDecimal, taxDetails: TaxDetails): Boolean =
    reclaimAmount > 0 && reclaimAmount <= BigDecimal(taxDetails.amount)

  def submitAmountForReclaim(
    securityDepositId: String,
    taxCode: TaxCode,
    reclaimAmount: BigDecimal
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (!isValidSecurityDepositId(securityDepositId))
        Left("submitAmountForReclaim.invalidSecurityDepositId")
      else if (!isSelectedDepositId(securityDepositId))
        Left("submitAmountForReclaim.securityDepositIdNotSelected")
      else if (!getSelectedDutiesFor(securityDepositId).exists(_.contains(taxCode)))
        Left("submitAmountForReclaim.invalidTaxCode")
      else if (!getSecurityTaxDetailsFor(securityDepositId, taxCode).exists(isValidReclaimAmount(reclaimAmount, _)))
        Left("submitAmountForReclaim.invalidAmount")
      else {
        val updatedSecuritiesReclaims: Option[SortedMap[String, SecuritiesReclaims]] =
          answers.securitiesReclaims.map(_.map {
            case (sid, reclaims) if sid === securityDepositId =>
              (
                sid,
                reclaims.map {
                  case (tc, _) if tc === taxCode => (tc, Some(reclaimAmount))
                  case other                     => other
                }
              )

            case other => other
          })

        Right(
          new SecuritiesJourney(
            answers.copy(
              securitiesReclaims = updatedSecuritiesReclaims
            )
          )
        )
      }
    }

  def submitFullAmountsForReclaim(securityDepositId: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      if (!isValidSecurityDepositId(securityDepositId))
        Left("submitFullAmountForReclaim.invalidSecurityDepositId")
      else if (!isSelectedDepositId(securityDepositId))
        Left("submitFullAmountForReclaim.securityDepositIdNotSelected")
      else {
        val updatedSecuritiesReclaims: Option[SortedMap[String, SecuritiesReclaims]] =
          for {
            securitiesReclaims <- answers.securitiesReclaims
            securityDetails    <- getSecurityDetailsFor(securityDepositId)
          } yield securitiesReclaims + {
            val fullAmountReclaims: SecuritiesReclaims =
              SortedMap(
                securityDetails.taxDetails
                  .map(td => TaxCodes.findUnsafe(td.taxType) -> Some(BigDecimal(td.amount))): _*
              )
            (
              securityDepositId,
              fullAmountReclaims
            )
          }

        Right(
          new SecuritiesJourney(
            answers.copy(
              securitiesReclaims = updatedSecuritiesReclaims
            )
          )
        )
      }
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new SecuritiesJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
            )
          )
        else Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, SecuritiesJourney] =
    if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
      Right(new SecuritiesJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber))))
    else
      Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): SecuritiesJourney =
    whileClaimIsAmendable {
      new SecuritiesJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): SecuritiesJourney =
    whileClaimIsAmendable {
      new SecuritiesJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new SecuritiesJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new SecuritiesJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          new SecuritiesJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): SecuritiesJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => new SecuritiesJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendableAnd(userCanProceedWithThisClaim) {
      validate(this)
        .fold(
          errors => Left(errors.headOption.getOrElse("completeWith.invalidJourney")),
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

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"SecuritiesJourney($answers, $caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], SecuritiesJourney.Output] =
    validate(this)
      .flatMap(_ =>
        (for {
          mrn                 <- getLeadMovementReferenceNumber
          rfs                 <- getReasonForSecurity
          supportingEvidences  = answers.supportingEvidences
          claimantInformation <- getClaimantInformation
        } yield SecuritiesJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          claimantInformation = claimantInformation,
          reasonForSecurity = rfs,
          securitiesReclaims = getSecuritiesReclaims,
          bankAccountDetails = answers.bankAccountDetails,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from)
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

  def prettyPrint: String = Json.prettyPrint(Json.toJson(this))

}

object SecuritiesJourney extends FluentImplicits[SecuritiesJourney] {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): SecuritiesJourney =
    new SecuritiesJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type SecuritiesReclaims = SortedMap[TaxCode, Option[BigDecimal]]

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    reasonForSecurity: Option[ReasonForSecurity] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    similarClaimExistAlreadyInCDFPay: Option[Boolean] = None, // TPI04 check flag
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    exportMovementReferenceNumber: Option[MRN] =
      None, // mandatory if reasonForSecurity is T/A, see ReasonForSecurity.requiresExportDeclaration
    exportDeclaration: Option[DEC91Response] = None, // mandatory as above
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    reclaimingFullAmount: Option[Boolean] = None,
    securitiesReclaims: Option[SortedMap[String, SecuritiesReclaims]] = None, // mandatory if NOT reclaimingFullAmount
    selectedDocumentType: Option[UploadDocumentType] = None, // ??? depending on the RfS and ....
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    checkYourAnswersChangeMode: Boolean = false
  ) extends CommonAnswers

  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    reasonForSecurity: ReasonForSecurity,
    securitiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]],
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import JourneyValidationErrors._
  import com.github.arturopala.validator.Validator._

  object Checks {

    val hasMovementReferenceNumber: Check[SecuritiesJourney] =
      Check(
        journey => journey.answers.movementReferenceNumber.isDefined,
        MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER
      )

    val hasDisplayDeclaration: Check[SecuritiesJourney] =
      Check(
        journey => journey.answers.displayDeclaration.isDefined,
        MISSING_DISPLAY_DECLARATION
      )

    val hasReasonForSecurity: Check[SecuritiesJourney] =
      Check(journey => journey.getReasonForSecurity.isDefined, MISSING_REASON_FOR_SECURITY)

    val hasMRNAndDisplayDeclarationAndRfS: Check[SecuritiesJourney] =
      hasMovementReferenceNumber &&
        hasDisplayDeclaration &&
        hasReasonForSecurity

    val canContinueTheClaimWithChoosenRfS: Check[SecuritiesJourney] =
      Check(
        journey => !journey.requiresExportDeclaration || journey.goodsHasBeenAlreadyExported,
        CHOOSEN_REASON_FOR_SECURITY_REQUIRES_GOODS_TO_BE_ALREADY_EXPORTED
      )

    val thereIsNoSimilarClaimInCDFPay: Check[SecuritiesJourney] =
      Check[SecuritiesJourney](
        _.answers.similarClaimExistAlreadyInCDFPay.isDefined,
        MISSING_CLAIM_DUPLICATE_CHECK_STATUS_WITH_TPI04
      ) && Check[SecuritiesJourney](
        _.answers.similarClaimExistAlreadyInCDFPay.contains(false),
        SIMILAR_CLAIM_EXISTS_ALREADY_IN_CDFPAY
      )

    val userCanProceedWithThisClaim: Check[SecuritiesJourney] =
      hasMRNAndDisplayDeclarationAndRfS &&
        thereIsNoSimilarClaimInCDFPay &&
        canContinueTheClaimWithChoosenRfS

  }

  import Checks._

  implicit val validator: Validate[SecuritiesJourney] =
    Validator.all(
      userCanProceedWithThisClaim,
      check(_.hasCompleteSecuritiesReclaims, INCOMPLETE_SECURITIES_RECLAIMS),
      check(_.hasCompleteSupportingEvidences, INCOMPLETE_SUPPORTING_EVIDENCES),
      checkIsDefined(_.answers.contactDetails, MISSING_CONTACT_DETAILS),
      checkIsDefined(_.answers.contactAddress, MISSING_CONTACT_ADDRESS),
      check(_.getTotalReclaimAmount > 0, TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO),
      whenTrue(
        _.needsDeclarantAndConsigneeEoriSubmission,
        all(
          checkIsDefined(
            _.answers.declarantEoriNumber,
            DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED
          ),
          checkEquals(
            _.getDeclarantEoriFromACC14,
            _.answers.declarantEoriNumber,
            DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14
          ),
          checkIsDefined(
            _.answers.consigneeEoriNumber,
            CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED
          ),
          checkEquals(
            _.getConsigneeEoriFromACC14,
            _.answers.consigneeEoriNumber,
            CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14
          )
        )
      ),
      whenFalse(
        _.needsDeclarantAndConsigneeEoriSubmission,
        all(
          checkIsEmpty(
            _.answers.declarantEoriNumber,
            DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED
          ),
          checkIsEmpty(
            _.answers.consigneeEoriNumber,
            CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED
          )
        )
      ),
      whenTrue(
        _.needsBanksAccountDetailsSubmission,
        all(
          checkIsDefined(
            _.answers.bankAccountDetails,
            BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED
          )
        )
      ),
      whenFalse(
        _.needsBanksAccountDetailsSubmission,
        all(
          checkIsEmpty(
            _.answers.bankAccountDetails,
            BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED
          )
        )
      )
    )

  object Answers {
    implicit lazy val mapFormat1: Format[SortedMap[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatSortedWithOptionalValue[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]]] =
      MapFormat.formatSorted[String, SortedMap[TaxCode, Option[BigDecimal]]]

    implicit lazy val mapFormat3: Format[Map[UploadDocumentType, (Nonce, Seq[UploadedFile])]] =
      MapFormat.format[UploadDocumentType, (Nonce, Seq[UploadedFile])]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.using[Json.WithDefaultValues].format[Answers]
  }

  object Output {

    implicit lazy val mapFormat1: Format[SortedMap[TaxCode, BigDecimal]] =
      MapFormat.formatSorted[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[SortedMap[String, SortedMap[TaxCode, BigDecimal]]] =
      MapFormat.formatSorted[String, SortedMap[TaxCode, BigDecimal]]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Output]   = Eq.fromUniversalEquals[Output]
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

  implicit val equality: Eq[SecuritiesJourney] =
    Eq.fromUniversalEquals[SecuritiesJourney]

}
