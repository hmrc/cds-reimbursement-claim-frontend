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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

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
  val caseNumber: Option[String] = None
) extends JourneyBase
    with DirectFluentSyntax[OverpaymentsScheduledJourney]
    with OverpaymentsJourneyProperties
    with CanSubmitMrnAndDeclaration {

  type Type = OverpaymentsScheduledJourney

  val self: OverpaymentsScheduledJourney = this

  val validate: Validator.Validate[OverpaymentsScheduledJourney] =
    OverpaymentsScheduledJourney.validator

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims
      .exists(rc =>
        rc.forall { case (dutyType, claims) =>
          claims.nonEmpty && claims.forall {
            case (taxCode, Some(claimAmounts)) =>
              dutyType.taxCodes.contains(taxCode) &&
                claimAmounts.isValid
            case _                             => false
          }
        }
      )

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean =
    true

  def getSelectedDocumentType: Option[UploadDocumentType] =
    answers.selectedDocumentType

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getSelectedDutyTypes: Option[Seq[DutyType]] =
    answers.reimbursementClaims.map(_.keys.toSeq)

  def getSelectedDuties: SortedMap[DutyType, Seq[TaxCode]] =
    answers.reimbursementClaims.map(_.mapValues(_.keys.toSeq)).getOrElse(SortedMap.empty)

  def getSelectedDutiesFor(dutyType: DutyType): Option[Seq[TaxCode]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType).map(_._2.keys.toSeq))

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

  def findNextDutyToSelectTaxCodes: Option[DutyType] =
    answers.reimbursementClaims.flatMap(_.find(_._2.isEmpty).map(_._1))

  val isDutyTypeSelected: Boolean = answers.reimbursementClaims.exists(_.nonEmpty)

  def getReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]] =
    answers.reimbursementClaims
      .map(_.mapValues(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) }))
      .getOrElse(SortedMap.empty)

  def getReimbursementFor(
    dutyType: DutyType,
    taxCode: TaxCode
  ): Option[AmountPaidWithCorrect] =
    getReimbursementClaimsFor(dutyType).flatMap(_.find(_._1 === taxCode)).flatMap(_._2)

  def getReimbursementClaimsFor(dutyType: DutyType): Option[SortedMap[TaxCode, Option[AmountPaidWithCorrect]]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType)).map(_._2)

  def getNextNdrcDetailsToClaim: Option[NdrcDetails] =
    answers.reimbursementClaims
      .flatMap(
        _.values
          .flatMap(_.toSeq)
          .collectFirst { case (taxCode: TaxCode, None) => taxCode }
          .flatMap(getNdrcDetailsFor)
      )

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.iterator.flatMap(_._2.map(_._2.refundAmount)).sum

  def getTotalPaidAmount: BigDecimal =
    getReimbursementClaims.iterator.flatMap(_._2.map(_._2.paidAmount)).sum

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsScheduledDocumentTypes)

  override def getAvailableClaimTypes: BasisOfOverpaymentClaimsList =
    BasisOfOverpaymentClaimsList()
      .excludeNorthernIrelandClaims(answers.whetherNorthernIreland.getOrElse(false), answers.displayDeclaration)

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
              s"submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
            )
          else
            Right(
              new OverpaymentsScheduledJourney(
                OverpaymentsScheduledJourney
                  .Answers(
                    nonce = answers.nonce,
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    displayDeclaration = Some(displayDeclaration)
                  )
              )
            )
      }
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new OverpaymentsScheduledJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
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
            new OverpaymentsScheduledJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
          )
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(answers.copy(basisOfClaim = Some(basisOfClaim)))
    }

  def submitWhetherNorthernIreland(whetherNorthernIreland: Boolean): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
        answers.copy(whetherNorthernIreland = Some(whetherNorthernIreland))
      )
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsScheduledJourney                 =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
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
        Right(new OverpaymentsScheduledJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
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
          Right(new OverpaymentsScheduledJourney(answers.copy(reimbursementClaims = newReimbursementClaims)))
        } else
          Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

  def isDutySelected(dutyType: DutyType, taxCode: TaxCode): Boolean =
    answers.reimbursementClaims
      .exists(_.exists { case (dt, tca) => dt === dutyType && tca.exists(_._1 === taxCode) })

  def submitAmountForReimbursement(
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
              answers.reimbursementClaims
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
            Right(new OverpaymentsScheduledJourney(answers.copy(reimbursementClaims = newReimbursementClaims)))
          } else
            Left("submitAmountForReimbursement.invalidReimbursementAmount")
        } else
          Left("submitAmountForReimbursement.taxCodeNotSelected")
      } else
        Left("submitAmountForReimbursement.taxCodeNotMatchingDutyType")
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      Right(
        new OverpaymentsScheduledJourney(
          answers.copy(bankAccountDetails = Some(bankAccountDetails))
        )
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      Right(
        new OverpaymentsScheduledJourney(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          new OverpaymentsScheduledJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => new OverpaymentsScheduledJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ => Right(new OverpaymentsScheduledJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsScheduledJourney =
    new OverpaymentsScheduledJourney(answers.copy(dutiesChangeMode = enabled))

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveScheduledDocument(
    requestNonce: Nonce,
    uploadedFile: UploadedFile
  ): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        Right(
          new OverpaymentsScheduledJourney(answers.copy(scheduledDocument = Some(uploadedFile)))
        )
      } else Left("receiveScheduledDocument.invalidNonce")
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def removeScheduledDocument: OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(answers.copy(scheduledDocument = None))
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
          mrn                    <- getLeadMovementReferenceNumber
          basisOfClaim           <- answers.basisOfClaim
          additionalDetails      <- answers.additionalDetails
          scheduledDocument      <- answers.scheduledDocument
          claimantInformation    <- getClaimantInformation
          whetherNorthernIreland <- answers.whetherNorthernIreland
        } yield OverpaymentsScheduledJourney.Output(
          movementReferenceNumber = mrn,
          scheduledDocument = EvidenceDocument.from(scheduledDocument),
          claimantType = getClaimantType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          whetherNorthernIreland = whetherNorthernIreland,
          additionalDetails = additionalDetails,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences = answers.supportingEvidences.map(EvidenceDocument.from),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object OverpaymentsScheduledJourney extends JourneyCompanion[OverpaymentsScheduledJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): OverpaymentsScheduledJourney =
    new OverpaymentsScheduledJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type ReimbursementClaims = SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]]

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    scheduledDocument: Option[UploadedFile] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfOverpaymentClaim] = None,
    whetherNorthernIreland: Option[Boolean] = None,
    additionalDetails: Option[String] = None,
    reimbursementClaims: Option[ReimbursementClaims] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    dutiesChangeMode: Boolean = false,
    checkYourAnswersChangeMode: Boolean = false
  ) extends OverpaymentsAnswers

  final case class Output(
    movementReferenceNumber: MRN,
    scheduledDocument: EvidenceDocument,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    whetherNorthernIreland: Boolean,
    additionalDetails: String,
    reimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import JourneyValidationErrors._
  import com.github.arturopala.validator.Validator._

  object Checks extends OverpaymentsJourneyChecks[OverpaymentsScheduledJourney] {}

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsScheduledJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      additionalDetailsHasBeenProvided,
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

  implicit val format: Format[OverpaymentsScheduledJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new OverpaymentsScheduledJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  /** Try to build journey from the pre-existing answers. */
  override def tryBuildFrom(answers: Answers): Either[String, OverpaymentsScheduledJourney] =
    empty(answers.userEoriNumber, answers.nonce)
      .flatMapWhenDefined(
        answers.movementReferenceNumber.zip(answers.displayDeclaration)
      )(j => { case (mrn: MRN, decl: DisplayDeclaration) =>
        j.submitMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .flatMapWhenDefined(answers.consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(answers.declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(answers.contactDetails))
      .flatMapWhenDefined(answers.scheduledDocument)(j => d => j.receiveScheduledDocument(j.answers.nonce, d))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .mapWhenDefined(answers.whetherNorthernIreland)(_.submitWhetherNorthernIreland)
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapWhenDefined(answers.reimbursementClaims.map(_.keySet.toSeq))(
        _.selectAndReplaceDutyTypeSetForReimbursement
      )
      .flatMapEachWhenDefined(answers.reimbursementClaims)(j => { case (dutyType, reimbursements) =>
        j.selectAndReplaceTaxCodeSetForReimbursement(dutyType, reimbursements.keySet.toSeq)
          .flatMapEachWhenMappingDefined(reimbursements)(j => {
            case (taxCode, AmountPaidWithCorrect(paidAmount, correctAmount)) =>
              j.submitAmountForReimbursement(dutyType, taxCode, paidAmount, correctAmount)
          })
      })
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
