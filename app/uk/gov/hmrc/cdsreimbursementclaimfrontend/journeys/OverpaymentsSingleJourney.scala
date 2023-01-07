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
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
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
import com.github.arturopala.validator.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList

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
  val caseNumber: Option[String] = None
) extends JourneyBase
    with DirectFluentSyntax[OverpaymentsSingleJourney]
    with OverpaymentsJourneyProperties
    with CanSubmitMrnAndDeclaration {

  type Type = OverpaymentsSingleJourney

  val self: OverpaymentsSingleJourney = this

  val validate: Validator.Validate[OverpaymentsSingleJourney] =
    OverpaymentsSingleJourney.validator

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(rc => rc.nonEmpty && rc.forall(_._2.isDefined))

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean =
    answers.reimbursementMethod.isEmpty ||
      answers.reimbursementMethod.contains(ReimbursementMethod.BankAccountTransfer)

  def needsDuplicateMrnAndDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry)

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getAvailableDuties: Seq[(TaxCode, Boolean)] =
    getNdrcDetails
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

  def getSelectedDuties: Option[Seq[TaxCode]] =
    answers.reimbursementClaims.map(_.keys.toSeq)

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.reimbursementClaims
      .map(_.keySet.map(getNdrcDetailsFor).collect { case Some(d) => d })
      .exists(_.forall(_.isCmaEligible))

  def getReimbursementClaims: Map[TaxCode, BigDecimal] =
    answers.reimbursementClaims
      .map(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) })
      .getOrElse(Map.empty)

  def getNextNdrcDetailsToClaim: Option[NdrcDetails] =
    answers.reimbursementClaims
      .flatMap(
        _.collectFirst { case (taxCode, None) => taxCode }
          .flatMap(getNdrcDetailsFor)
      )

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.toSeq.map(_._2).sum

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsSingleDocumentTypes)

  override def getAvailableClaimTypes: BasisOfOverpaymentClaimsList =
    BasisOfOverpaymentClaimsList()
      .excludeNorthernIrelandClaims(answers.whetherNorthernIreland.getOrElse(false), answers.displayDeclaration)

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
              s"submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
            )
          else
            Right(
              new OverpaymentsSingleJourney(
                OverpaymentsSingleJourney
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

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new OverpaymentsSingleJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
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
            new OverpaymentsSingleJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
          )
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      new OverpaymentsSingleJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      new OverpaymentsSingleJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitWhetherNorthernIreland(whetherNorthernIreland: Boolean): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      new OverpaymentsSingleJourney(
        answers.copy(whetherNorthernIreland = Some(whetherNorthernIreland))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      basisOfClaim match {
        case BasisOfOverpaymentClaim.DuplicateEntry =>
          new OverpaymentsSingleJourney(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case _ =>
          new OverpaymentsSingleJourney(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              duplicateMovementReferenceNumber = None,
              duplicateDisplayDeclaration = None
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
            s"submitDuplicateMovementReferenceNumberAndDeclaration.mustBeDifferent"
          )
        case _                                                 =>
          if (duplicateMrn =!= duplicateDisplayDeclaration.getMRN)
            Left(
              s"submitDuplicateMovementReferenceNumberAndDeclaration.wrongDisplayDeclarationMrn"
            )
          else
            Right(
              new OverpaymentsSingleJourney(
                answers.copy(
                  duplicateMovementReferenceNumber = Some(duplicateMrn),
                  duplicateDisplayDeclaration = Some(duplicateDisplayDeclaration)
                )
              )
            )
      }
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      new OverpaymentsSingleJourney(
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
              val newReimbursementClaims = answers.reimbursementClaims match {
                case None                      =>
                  Map(taxCodes.map(taxCode => taxCode -> None): _*)

                case Some(reimbursementClaims) =>
                  Map(taxCodes.map { taxCode =>
                    taxCode -> reimbursementClaims.get(taxCode).flatten
                  }: _*)
              }
              Right(new OverpaymentsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
            } else
              Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
          }
      }
    }

  def isValidReimbursementAmount(reimbursementAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    reimbursementAmount > 0 && reimbursementAmount <= BigDecimal(ndrcDetails.amount)

  def submitAmountForReimbursement(
    taxCode: TaxCode,
    reimbursementAmount: BigDecimal
  ): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      getLeadDisplayDeclaration match {
        case None =>
          Left("submitAmountForReimbursement.missingDisplayDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(taxCode) match {
            case None =>
              Left("submitAmountForReimbursement.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidReimbursementAmount(reimbursementAmount, ndrcDetails) =>
              if (getSelectedDuties.exists(_.contains(taxCode))) {
                val newReimbursementClaims = answers.reimbursementClaims match {
                  case None                      => Map(taxCode -> Some(reimbursementAmount))
                  case Some(reimbursementClaims) => reimbursementClaims + (taxCode -> Some(reimbursementAmount))
                }
                Right(new OverpaymentsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
              } else
                Left("submitAmountForReimbursement.taxCodeNotSelectedYet")

            case _ =>
              Left("submitAmountForReimbursement.invalidReimbursementAmount")
          }
      }
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new OverpaymentsSingleJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new OverpaymentsSingleJourney(
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
            new OverpaymentsSingleJourney(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = None
              )
            )
          )
        else
          Right(
            new OverpaymentsSingleJourney(
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
      new OverpaymentsSingleJourney(
        answers.copy(
          reimbursementMethod = None,
          bankAccountType = None,
          bankAccountDetails = None
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      new OverpaymentsSingleJourney(answers.copy(selectedDocumentType = Some(documentType)))
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
          new OverpaymentsSingleJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsSingleJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => new OverpaymentsSingleJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, OverpaymentsSingleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ => Right(new OverpaymentsSingleJourney(answers = this.answers, caseNumber = Some(caseNumber)))
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
        } yield OverpaymentsSingleJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          whetherNorthernIreland = whetherNorthernIreland,
          additionalDetails = additionalDetails,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          duplicateMovementReferenceNumber = answers.duplicateMovementReferenceNumber,
          reimbursementMethod = answers.reimbursementMethod.getOrElse(ReimbursementMethod.BankAccountTransfer),
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object OverpaymentsSingleJourney extends JourneyCompanion[OverpaymentsSingleJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): OverpaymentsSingleJourney =
    new OverpaymentsSingleJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type ReimbursementClaims = Map[TaxCode, Option[BigDecimal]]

  // All user answers captured during C&E1179 single MRN journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    duplicateMovementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    duplicateDisplayDeclaration: Option[DisplayDeclaration] = None,
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
    reimbursementMethod: Option[ReimbursementMethod] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = false
  ) extends OverpaymentsAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    duplicateMovementReferenceNumber: Option[MRN],
    claimantType: ClaimantType,
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
          _.isAllSelectedDutiesAreCMAEligible,
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
              _.answers.duplicateMovementReferenceNumber,
              DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED
            ),
            checkIsDefined(
              _.answers.duplicateDisplayDeclaration,
              DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED
            )
          )
        ),
        whenFalse(
          _.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry),
          all(
            checkIsEmpty(
              _.answers.duplicateMovementReferenceNumber,
              DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_NOT_BE_DEFINED
            ),
            checkIsEmpty(
              _.answers.duplicateDisplayDeclaration,
              DUPLICATE_DISPLAY_DECLARATION_MUST_NOT_BE_DEFINED
            )
          )
        )
      )

  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsSingleJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      duplicateMovementReferenceNumberHasBeenProvidedIfNeeded,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      reimbursementMethodHasBeenProvidedIfNeeded,
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

  implicit val format: Format[OverpaymentsSingleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new OverpaymentsSingleJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  /** Try to build journey from the pre-existing answers. */
  override def tryBuildFrom(answers: Answers): Either[String, OverpaymentsSingleJourney] =
    empty(answers.userEoriNumber, answers.nonce)
      .flatMapWhenDefined(
        answers.movementReferenceNumber.zip(answers.displayDeclaration)
      )(j => { case (mrn: MRN, decl: DisplayDeclaration) =>
        j.submitMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .flatMapWhenDefined(answers.consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(answers.declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .mapWhenDefined(answers.whetherNorthernIreland)(_.submitWhetherNorthernIreland)
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .flatMapWhenDefined(answers.duplicateMovementReferenceNumber.zip(answers.duplicateDisplayDeclaration))(j => {
        case (mrn: MRN, decl: DisplayDeclaration) =>
          j.submitDuplicateMovementReferenceNumberAndDeclaration(mrn, decl)
      })
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapWhenDefined(answers.reimbursementClaims.map(_.keySet.toSeq))(
        _.selectAndReplaceTaxCodeSetForReimbursement
      )
      .flatMapEachWhenDefinedAndMappingDefined(answers.reimbursementClaims)(_.submitAmountForReimbursement)
      .flatMapWhenDefined(answers.reimbursementMethod)(_.submitReimbursementMethod)
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
