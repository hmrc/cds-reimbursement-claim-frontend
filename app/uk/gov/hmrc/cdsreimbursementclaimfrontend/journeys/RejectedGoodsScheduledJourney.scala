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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentImplicits
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

import java.time.LocalDate
import scala.collection.immutable.ListMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement

/** An encapsulated C&E1179 scheduled MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsScheduledJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[RejectedGoodsScheduledJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class RejectedGoodsScheduledJourney private (
  val answers: RejectedGoodsScheduledJourney.Answers,
  val caseNumber: Option[String] = None
) extends RejectedGoods.CommonJourneyProperties
    with FluentSyntax[RejectedGoodsScheduledJourney] {

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def hasCompleteAnswers: Boolean =
    RejectedGoodsScheduledJourney.validator.apply(this).isValid

  /** Check if all the selected duties have reimbursement and paid amounts provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims
      .exists(rc =>
        rc.exists(_._2.nonEmpty) && rc.forall { case (dutyType, claims) =>
          claims.nonEmpty && claims.forall {
            case (taxCode, Some(claimAmounts)) =>
              dutyType.taxCodes.contains(taxCode) &&
                isValidReimbursementAmount(
                  claimAmounts.shouldOfPaid,
                  claimAmounts.paidAmount
                )
            case _                             => false
          }
        }
      )

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean = true

  def getSelectedDutyTypes: Option[Seq[DutyType]] =
    answers.reimbursementClaims.map(_.keys.toSeq)

  def getSelectedDuties: Map[DutyType, Seq[TaxCode]] =
    answers.reimbursementClaims.map(_.mapValues(_.keys.toSeq)).getOrElse(Map.empty)

  def getSelectedDutiesFor(dutyType: DutyType): Option[Seq[TaxCode]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType).map(_._2.keys.toSeq))

  def getReimbursementClaimsFor(
    dutyType: DutyType
  ): Option[Map[TaxCode, Option[Reimbursement]]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType)).map(_._2)

  def isDutySelected(taxCode: TaxCode): Boolean =
    answers.reimbursementClaims
      .exists(_.exists { case (dutyType, tca) =>
        dutyType.taxCodes.contains(taxCode) && tca.exists(_._1 === taxCode)
      })

  def getReimbursementClaims: Map[TaxCode, Reimbursement] =
    answers.reimbursementClaims
      .map(rc => rc.flatMap(_._2.collect { case (taxCode, Some(claimAmounts)) => (taxCode, claimAmounts) }))
      .getOrElse(Map.empty)

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.iterator.map(_._2.shouldOfPaid).sum

  def getTotalPaidAmount: BigDecimal =
    getReimbursementClaims.toSeq.map(_._2.paidAmount).sum

  def isFinalized: Boolean = caseNumber.isDefined

  def whileJourneyIsAmendable(body: => RejectedGoodsScheduledJourney): RejectedGoodsScheduledJourney =
    if (isFinalized) this else body

  def whileJourneyIsAmendable(
    body: => Either[String, RejectedGoodsScheduledJourney]
  ): Either[String, RejectedGoodsScheduledJourney] =
    if (isFinalized) Left(RejectedGoods.ValidationErrors.JOURNEY_ALREADY_FINALIZED) else body

  /** Resets the journey with the new MRN
    * or keep existing journey if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn)
            if existingMrn === mrn &&
              getLeadDisplayDeclaration.contains(displayDeclaration) =>
          Right(this)
        case _ =>
          if (mrn =!= displayDeclaration.getMRN)
            Left(
              s"submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
            )
          else
            Right(
              new RejectedGoodsScheduledJourney(
                RejectedGoodsScheduledJourney
                  .Answers(
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    displayDeclaration = Some(displayDeclaration),
                    nonce = answers.nonce
                  )
              )
            )
      }
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new RejectedGoodsScheduledJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
            )
          )
        else Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
          Right(
            new RejectedGoodsScheduledJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
          )
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      basisOfClaim match {
        case BasisOfRejectedGoodsClaim.SpecialCircumstances =>
          new RejectedGoodsScheduledJourney(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case _ =>
          new RejectedGoodsScheduledJourney(
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
    whileJourneyIsAmendable {
      answers.basisOfClaim match {
        case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
          Right(
            new RejectedGoodsScheduledJourney(
              answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstancesDetails))
            )
          )
        case _                                                    =>
          Left("basisOfClaim.not_matching")
      }
    }

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceDutyTypeSetForReimbursement(
    dutyTypes: Seq[DutyType]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (dutyTypes.isEmpty)
        Left("selectAndReplaceDutyTypeSetForReimbursement.emptySelection")
      else {
        val newReimbursementClaims = ListMap(
          dutyTypes
            .map(dutyType => (dutyType -> getReimbursementClaimsFor(dutyType).getOrElse(Map.empty))): _*
        )
        Right(new RejectedGoodsScheduledJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
      }
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    dutyType: DutyType,
    taxCodes: Seq[TaxCode]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (!getSelectedDutyTypes.exists(_.contains(dutyType)))
        Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      else if (taxCodes.isEmpty)
        Left("selectTaxCodeSetForReimbursement.emptySelection")
      else {
        val allTaxCodesMatchDutyType = taxCodes.forall(tc => dutyType.taxCodes.contains(tc))
        if (allTaxCodesMatchDutyType) {
          val newReimbursementClaims =
            answers.reimbursementClaims
              .map { rc =>
                ListMap(rc.toSeq.map {
                  case (dt, reimbursementClaims) if dt.repr === dutyType.repr =>
                    dt -> ListMap(taxCodes.map { tc =>
                      tc -> reimbursementClaims.get(tc).flatten
                    }: _*)
                  case pair                                                   => pair
                }: _*)
              }
          Right(new RejectedGoodsScheduledJourney(answers.copy(reimbursementClaims = newReimbursementClaims)))
        } else
          Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

  def isValidReimbursementAmount(reimbursementAmount: BigDecimal, paidAmount: BigDecimal): Boolean =
    reimbursementAmount > 0 && reimbursementAmount <= paidAmount

  def submitAmountForReimbursement(
    taxCode: TaxCode,
    reimbursementAmount: BigDecimal,
    paidAmount: BigDecimal
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (isValidReimbursementAmount(reimbursementAmount, paidAmount)) {
        if (isDutySelected(taxCode)) {
          val newReimbursementClaims =
            answers.reimbursementClaims
              .map(rc =>
                ListMap(rc.toSeq.map { case (dt, reimbursementClaims) =>
                  dt -> ListMap(reimbursementClaims.toSeq.map {
                    case (tc, _) if tc === taxCode =>
                      tc -> Some(Reimbursement(paidAmount, reimbursementAmount))
                    case pair                      => pair
                  }: _*)
                }: _*)
              )
          Right(new RejectedGoodsScheduledJourney(answers.copy(reimbursementClaims = newReimbursementClaims)))
        } else
          Left(s"submitAmountForReimbursement.taxCodeNotSelected")
      } else
        Left("submitAmountForReimbursement.invalidReimbursementAmount")
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsScheduledJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsScheduledJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveScheduledDocument(
    requestNonce: Nonce,
    uploadedFile: UploadedFile
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        Right(
          new RejectedGoodsScheduledJourney(answers.copy(scheduledDocument = Some(uploadedFile)))
        )
      } else Left("receiveScheduledDocument.invalidNonce")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsScheduledJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          new RejectedGoodsScheduledJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsScheduledJourney =
    whileJourneyIsAmendable {
      RejectedGoodsScheduledJourney.validator
        .apply(this)
        .fold(
          _ => this,
          _ => new RejectedGoodsScheduledJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsScheduledJourney] =
    whileJourneyIsAmendable {
      RejectedGoodsScheduledJourney.validator
        .apply(this)
        .toEither
        .fold(
          errors => Left(errors.headOption.getOrElse("completeWith.invalidJourney")),
          _ => Right(new RejectedGoodsScheduledJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[RejectedGoodsScheduledJourney]) {
      val that = obj.asInstanceOf[RejectedGoodsScheduledJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"RejectedGoodsScheduledJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], RejectedGoodsScheduledJourney.Output] =
    RejectedGoodsScheduledJourney.validator
      .apply(this)
      .toEither
      .flatMap(_ =>
        (for {
          mrn                    <- getLeadMovementReferenceNumber
          basisOfClaim           <- answers.basisOfClaim
          methodOfDisposal       <- answers.methodOfDisposal
          detailsOfRejectedGoods <- answers.detailsOfRejectedGoods
          inspectionDate         <- answers.inspectionDate
          inspectionAddress      <- answers.inspectionAddress
          supportingEvidences     = answers.supportingEvidences
          scheduledDocument      <- answers.scheduledDocument
          claimantInformation    <- getClaimantInformation
        } yield RejectedGoodsScheduledJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          methodOfDisposal = methodOfDisposal,
          detailsOfRejectedGoods = detailsOfRejectedGoods,
          inspectionDate = inspectionDate,
          inspectionAddress = inspectionAddress,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences =
            EvidenceDocument.from(scheduledDocument) +: supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod = ReimbursementMethodAnswer.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

  def prettyPrint: String = Json.prettyPrint(Json.toJson(this))

}

object RejectedGoodsScheduledJourney extends FluentImplicits[RejectedGoodsScheduledJourney] {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): RejectedGoodsScheduledJourney =
    new RejectedGoodsScheduledJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type ReimbursementClaims = Map[DutyType, Map[TaxCode, Option[Reimbursement]]]

  // All user answers captured during C&E1179 scheduled MRN journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    reimbursementClaims: Option[ReimbursementClaims] = None,
    inspectionDate: Option[InspectionDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    scheduledDocument: Option[UploadedFile] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = false
  ) extends RejectedGoods.CommonAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    methodOfDisposal: MethodOfDisposal,
    detailsOfRejectedGoods: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: Map[TaxCode, Reimbursement],
    reimbursementMethod: ReimbursementMethodAnswer,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._
  import RejectedGoods.ValidationErrors._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  val validator: Validate[RejectedGoodsScheduledJourney] =
    all(
      checkIsDefined(_.getLeadMovementReferenceNumber, MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER),
      checkIsDefined(_.getLeadDisplayDeclaration, MISSING_DISPLAY_DECLARATION),
      checkIsDefined(_.answers.basisOfClaim, MISSING_BASIS_OF_CLAIM),
      checkIsDefined(_.answers.detailsOfRejectedGoods, MISSING_DETAILS_OF_REJECTED_GOODS),
      checkIsDefined(_.answers.inspectionDate, MISSING_INSPECTION_DATE),
      checkIsDefined(_.answers.inspectionAddress, MISSING_INSPECTION_ADDRESS),
      checkIsDefined(_.answers.methodOfDisposal, MISSING_METHOD_OF_DISPOSAL),
      check(_.hasCompleteReimbursementClaims, INCOMPLETE_REIMBURSEMENT_CLAIMS),
      check(_.hasCompleteSupportingEvidences, INCOMPLETE_SUPPORTING_EVIDENCES),
      checkIsDefined(_.answers.contactDetails, MISSING_CONTACT_DETAILS),
      checkIsDefined(_.answers.contactAddress, MISSING_CONTACT_ADDRESS),
      checkIsDefined(_.answers.scheduledDocument, MISSING_SCHEDULED_DOCUMENT),
      check(_.getTotalReimbursementAmount > 0, TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO),
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
      ),
      whenTrue(
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        checkIsDefined(
          _.answers.basisOfClaimSpecialCircumstances,
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED
        )
      ),
      whenFalse(
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        checkIsEmpty(
          _.answers.basisOfClaimSpecialCircumstances,
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED
        )
      )
    )

  object Answers {

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    implicit lazy val dutyTypeFormat: Format[DutyType] =
      SimpleStringFormat[DutyType](
        repr =>
          DutyTypes
            .find(repr)
            .getOrElse(throw new Exception(s"Cannot parse duty type from the string [$repr]")),
        _.repr
      )

    implicit lazy val mapFormat1: Format[Map[TaxCode, Option[Reimbursement]]] =
      MapFormat.formatWithOptionalValue[TaxCode, Reimbursement]

    implicit lazy val mapFormat2: Format[ReimbursementClaims] =
      MapFormat.format[DutyType, Map[TaxCode, Option[Reimbursement]]]

    implicit lazy val mapFormat3: Format[Map[UploadDocumentType, (Nonce, Seq[UploadedFile])]] =
      MapFormat.format[UploadDocumentType, (Nonce, Seq[UploadedFile])]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.using[Json.WithDefaultValues].format[Answers]
  }

  object Output {

    implicit lazy val mapFormat1: Format[Map[TaxCode, Reimbursement]] =
      MapFormat.format[TaxCode, Reimbursement]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Output]   = Eq.fromUniversalEquals[Output]
    implicit val format: Format[Output] = Json.format[Output]
  }

  import play.api.libs.functional.syntax._

  implicit val format: Format[RejectedGoodsScheduledJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new RejectedGoodsScheduledJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  implicit val equality: Eq[RejectedGoodsScheduledJourney] =
    Eq.fromUniversalEquals[RejectedGoodsScheduledJourney]

}
