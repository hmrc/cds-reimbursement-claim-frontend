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
import cats.syntax.eq._
import com.github.arturopala.validator.Validator
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

import java.time.LocalDate
import scala.collection.immutable.SortedMap

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
) extends JourneyBase
    with DirectFluentSyntax[RejectedGoodsScheduledJourney]
    with RejectedGoodsJourneyProperties
    with CanSubmitMrnAndDeclaration
    with CanSubmitContactDetails
    with HaveInspectionDetails {

  type Type = RejectedGoodsScheduledJourney

  val self: RejectedGoodsScheduledJourney = this

  val validate: Validator.Validate[RejectedGoodsScheduledJourney] =
    RejectedGoodsScheduledJourney.validator

  /** Check if all the selected duties have reimbursement and paid amounts provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims
      .exists(rc =>
        rc.exists(_._2.nonEmpty) && rc.forall { case (dutyType, claims) =>
          claims.nonEmpty && claims.forall {
            case (taxCode, Some(claimAmounts)) =>
              dutyType.taxCodes.contains(taxCode) &&
                isValidClaimAmounts(
                  claimAmounts.refundAmount,
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

  def getSelectedDuties: SortedMap[DutyType, Seq[TaxCode]] =
    answers.reimbursementClaims
      .map(_.view.mapValues(_.keys.toSeq).to(SortedMap))
      .getOrElse(SortedMap.empty)

  def getSelectedDutiesFor(dutyType: DutyType): Option[Seq[TaxCode]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType).map(_._2.keys.toSeq))

  def getFirstDutyToClaim: Option[(DutyType, TaxCode)] =
    getSelectedDuties.headOption
      .flatMap { case (dt, tcs) =>
        tcs.headOption.map(tc => (dt, tc))
      }

  def findNextSelectedDutyAfter(dutyType: DutyType): Option[DutyType] =
    getSelectedDutyTypes.flatMap(nextAfter(dutyType) _)

  def findNextSelectedTaxCodeAfter(dutyType: DutyType, taxCode: TaxCode): Option[(DutyType, TaxCode)] =
    getSelectedDutiesFor(dutyType).flatMap(nextAfter(taxCode) _) match {
      case Some(taxCode) => Some((dutyType, taxCode))
      case None          =>
        findNextSelectedDutyAfter(dutyType)
          .flatMap(dt =>
            getSelectedDutiesFor(dt)
              .flatMap(_.headOption)
              .map(tc => (dt, tc))
          )
    }

  def findNextDutyToSelectDuties: Option[DutyType] =
    answers.reimbursementClaims.flatMap(_.find(_._2.isEmpty).map(_._1))

  def getReimbursementClaimsFor(
    dutyType: DutyType
  ): Option[SortedMap[TaxCode, Option[AmountPaidWithRefund]]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType)).map(_._2)

  def getReimbursementFor(
    dutyType: DutyType,
    taxCode: TaxCode
  ): Option[AmountPaidWithRefund] =
    getReimbursementClaimsFor(dutyType).flatMap(_.find(_._1 === taxCode)).flatMap(_._2)

  def isDutySelected(dutyType: DutyType, taxCode: TaxCode): Boolean =
    answers.reimbursementClaims
      .exists(_.exists { case (dt, tca) => dt === dutyType && tca.exists(_._1 === taxCode) })

  val isDutyTypeSelected: Boolean                                   = answers.reimbursementClaims.exists(_.nonEmpty)

  def getReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithRefund]] =
    answers.reimbursementClaims
      .map(_.view.mapValues(_.collect { case (tc, Some(r)) => (tc, r) }).to(SortedMap))
      .getOrElse(SortedMap.empty)

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.iterator.flatMap(_._2.map(_._2.refundAmount)).sum

  def getTotalPaidAmount: BigDecimal =
    getReimbursementClaims.iterator.flatMap(_._2.map(_._2.paidAmount)).sum

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsScheduledDocumentTypes)

  /** Resets the journey with the new MRN
    * or keep existing journey if submitted the same MRN and declaration as before.
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
          if (mrn =!= displayDeclaration.getMRN)
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
                  )
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (
          getConsigneeEoriFromACC14 match {
            case Some(eori) => eori === consigneeEoriNumber
            case None       => getDeclarantEoriFromACC14.contains(consigneeEoriNumber)
          }
        )
          Right(
            new RejectedGoodsScheduledJourney(
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
          Right(
            new RejectedGoodsScheduledJourney(
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

  def submitContactDetails(contactDetails: Option[MrnContactDetails]) =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress) =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceDutyTypeSetForReimbursement(
    dutyTypes: Seq[DutyType]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if (dutyTypes.isEmpty)
        Left("selectAndReplaceDutyTypeSetForReimbursement.emptySelection")
      else {
        val newReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithRefund]]] =
          SortedMap(
            dutyTypes
              .map(dutyType =>
                (dutyType -> getReimbursementClaimsFor(dutyType)
                  .getOrElse(SortedMap.empty[TaxCode, Option[AmountPaidWithRefund]]))
              ): _*
          )
        Right(new RejectedGoodsScheduledJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
      }
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    dutyType: DutyType,
    taxCodes: Seq[TaxCode]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
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
                SortedMap(rc.toSeq.map {
                  case (dt, reimbursementClaims) if dt.repr === dutyType.repr =>
                    dt -> SortedMap(taxCodes.map { tc =>
                      tc -> reimbursementClaims.get(tc).flatten
                    }: _*)
                  case other                                                  => other
                }: _*)
              }
          Right(new RejectedGoodsScheduledJourney(answers.copy(reimbursementClaims = newReimbursementClaims)))
        } else
          Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

  def isValidClaimAmounts(claimAmount: BigDecimal, paidAmount: BigDecimal): Boolean =
    paidAmount > 0 && claimAmount > 0 && claimAmount <= paidAmount

  def submitAmountForReimbursement(
    dutyType: DutyType,
    taxCode: TaxCode,
    claimAmount: BigDecimal,
    paidAmount: BigDecimal
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if (dutyType.taxCodes.contains(taxCode)) {
        if (isDutySelected(dutyType, taxCode)) {
          if (isValidClaimAmounts(claimAmount, paidAmount)) {
            val newReimbursementClaims =
              answers.reimbursementClaims
                .map(rc =>
                  SortedMap(rc.toSeq.map {
                    case (dt, reimbursementClaims) if dt === dutyType =>
                      dt -> SortedMap(reimbursementClaims.toSeq.map {
                        case (tc, _) if tc === taxCode =>
                          tc -> Some(AmountPaidWithRefund(paidAmount, claimAmount))
                        case other                     => other
                      }: _*)
                    case other                                        => other
                  }: _*)
                )
            Right(new RejectedGoodsScheduledJourney(answers.copy(reimbursementClaims = newReimbursementClaims)))
          } else
            Left("submitAmountForReimbursement.invalidReimbursementAmount")
        } else
          Left("submitAmountForReimbursement.taxCodeNotSelected")
      } else
        Left("submitAmountForReimbursement.taxCodeNotMatchingDutyType")
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsScheduledJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        Right(
          new RejectedGoodsScheduledJourney(answers.copy(scheduledDocument = Some(uploadedFile)))
        )
      } else Left("receiveScheduledDocument.invalidNonce")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def removeScheduledDocument: RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(answers.copy(scheduledDocument = None))
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsScheduledJourney =
    whileClaimIsAmendable {
      new RejectedGoodsScheduledJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => new RejectedGoodsScheduledJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsScheduledJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
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
  def toOutput: Either[Seq[String], RejectedGoodsScheduledJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for {
          mrn                    <- getLeadMovementReferenceNumber
          basisOfClaim           <- answers.basisOfClaim
          methodOfDisposal       <- answers.methodOfDisposal
          detailsOfRejectedGoods <- answers.detailsOfRejectedGoods
          inspectionDate         <- answers.inspectionDate
          inspectionAddress      <- answers.inspectionAddress
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
          scheduledDocument = EvidenceDocument.from(scheduledDocument),
          supportingEvidences = answers.supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsScheduledJourney extends JourneyCompanion[RejectedGoodsScheduledJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): RejectedGoodsScheduledJourney =
    new RejectedGoodsScheduledJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type ReimbursementClaims = SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithRefund]]]

  // All user answers captured during C&E1179 scheduled MRN journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
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
  ) extends RejectedGoodsAnswers

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
    reimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithRefund]],
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
      supportingEvidenceHasBeenProvided
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

  implicit val format: Format[RejectedGoodsScheduledJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new RejectedGoodsScheduledJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  override def tryBuildFrom(answers: Answers): Either[String, RejectedGoodsScheduledJourney] =
    empty(answers.userEoriNumber, answers.nonce)
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
      .flatMapWhenDefined(answers.scheduledDocument)(j => d => j.receiveScheduledDocument(j.answers.nonce, d))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .flatMapWhenDefined(answers.basisOfClaimSpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails
      )
      .mapWhenDefined(answers.methodOfDisposal)(_.submitMethodOfDisposal)
      .mapWhenDefined(answers.detailsOfRejectedGoods)(_.submitDetailsOfRejectedGoods)
      .flatMapWhenDefined(answers.reimbursementClaims.map(_.keySet.toSeq))(
        _.selectAndReplaceDutyTypeSetForReimbursement
      )
      .flatMapEachWhenDefined(answers.reimbursementClaims)(j => { case (dutyType, reimbursements) =>
        j.selectAndReplaceTaxCodeSetForReimbursement(dutyType, reimbursements.keySet.toSeq)
          .flatMapEachWhenMappingDefined(reimbursements)(j => {
            case (taxCode, AmountPaidWithRefund(paidAmount, claimAmount)) =>
              j.submitAmountForReimbursement(dutyType, taxCode, paidAmount, claimAmount)
          })
      })
      .mapWhenDefined(answers.inspectionDate)(_.submitInspectionDate)
      .mapWhenDefined(answers.inspectionAddress)(_.submitInspectionAddress)
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
