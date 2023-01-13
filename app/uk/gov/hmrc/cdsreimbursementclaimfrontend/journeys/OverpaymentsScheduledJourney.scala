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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{AmountPaidWithCorrect, BankAccountDetails, BankAccountType, BasisOfOverpaymentClaim, BasisOfOverpaymentClaimsList, ClaimantInformation, DutyType, EvidenceDocument, MrnContactDetails, Nonce, ReimbursementMethod, TaxCode, TaxCodes, UploadedFile}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{Eori, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

import scala.collection.immutable.{SortedMap, SortedSet, TreeMap}
import scala.collection.mutable

/** An encapsulated C&E1179 single MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[OverpaymentsScheduledJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[OverpaymentsScheduledJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class OverpaymentsScheduledJourney private(
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
        rc.exists(_._2.nonEmpty) && rc.forall {
          case (dutyType, claims) =>
            claims.nonEmpty && claims.forall {
              case (taxCode, Some(claimAmounts)) =>
                dutyType.taxCodes.contains(taxCode) &&
                  claimAmounts.isValid
              case _ => false
            }
        }
      )

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean =
    answers.reimbursementMethod.isEmpty ||
      answers.reimbursementMethod.contains(ReimbursementMethod.BankAccountTransfer)

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

  def getSelectedTaxCodes: Option[Seq[TaxCode]] =
    answers.reimbursementClaims.map(_.flatMap(_._2).keys.toSeq)

  def getSelectedDutyTypes: Option[Seq[DutyType]] =
    answers.reimbursementClaims.map(_.keys.toSeq)

  def getSelectedDutiesFor(dutyType: DutyType): Option[Seq[TaxCode]] =
    answers.reimbursementClaims.flatMap(_.find(_._1 === dutyType).map(_._2.keys.toSeq))


  private def nextAfter[A](item: A)(seq: Seq[A]): Option[A] = {
    val i = seq.indexOf(item)
    if (i === -1) None
    else if (i === seq.size - 1) None
    else Some(seq(i + 1))
  }

  def findNextSelectedDutyAfter(dutyType: DutyType): Option[DutyType] =
    getSelectedDutyTypes.flatMap(nextAfter(dutyType) _)

  def findNextSelectedTaxCodeAfter(dutyType: DutyType, taxCode: TaxCode): Option[(DutyType, TaxCode)] =
    getSelectedDutiesFor(dutyType).flatMap(nextAfter(taxCode) _) match {
      case Some(taxCode) => Some((dutyType, taxCode))
      case None =>
        findNextSelectedDutyAfter(dutyType)
          .flatMap(dt =>
            getSelectedDutiesFor(dt)
              .flatMap(_.headOption)
              .map(tc => (dt, tc))
          )
    }

  def findNextDutyToSelectTaxCodes: Option[DutyType] =
    answers.reimbursementClaims.flatMap(_.find(_._2.isEmpty).map(_._1))

  def isDutySelected(dutyType: DutyType, taxCode: TaxCode): Boolean =
    answers.reimbursementClaims
      .exists(_.exists { case (dt, tca) => dt === dutyType && tca.exists(_._1 === taxCode) })

  val isDutyTypeSelected: Boolean = answers.reimbursementClaims.exists(_.nonEmpty)


  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.reimbursementClaims
      .flatMap(_.values.map(_.keySet))
      .map(_.keySet.map(getNdrcDetailsFor).collect { case Some(d) => d })
      .exists(_.forall(_.isCmaEligible))

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

  def getReimbursementClaims: SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithCorrect]] =
    answers.reimbursementClaims
      .map(_.mapValues(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) }))
      .getOrElse(SortedMap.empty)

  def getNextNdrcDetailsToClaim: Option[NdrcDetails] =
    answers.reimbursementClaims
      .flatMap(_
        .values
        .flatMap(_.toSeq)
        .collectFirst { case (taxCode: TaxCode, None) => taxCode }
        .flatMap(getNdrcDetailsFor)
      )

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.flatMap(_._2).values.map(_.refundAmount).sum

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

  def submitWhetherNorthernIreland(whetherNorthernIreland: Boolean): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
        answers.copy(whetherNorthernIreland = Some(whetherNorthernIreland))
      )
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, OverpaymentsScheduledJourney] =
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
                    taxCode -> reimbursementClaims.flatMap(_._2).get(taxCode)
                  }: _*)
              }
              Right(new OverpaymentsScheduledJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
            } else
              Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
          }
      }
    }

  def isValidReimbursementAmount(reimbursementAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    reimbursementAmount > 0 && reimbursementAmount <= BigDecimal(ndrcDetails.amount)

  def isDutySelected(dutyType: DutyType, taxCode: TaxCode): Boolean =
    answers.reimbursementClaims
      .exists(_.exists { case (dt, tca) => dt === dutyType && tca.exists(_._1 === taxCode) })

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
              if (getSelectedTaxCodes.exists(_.contains(taxCode))) {
                val newReimbursementClaims = answers.reimbursementClaims match {
                  case None => Map(taxCode -> Some(reimbursementAmount))
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new OverpaymentsScheduledJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new OverpaymentsScheduledJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitReimbursementMethod(
    reimbursementMethod: ReimbursementMethod
  ): Either[String, OverpaymentsScheduledJourney] =
    whileClaimIsAmendable {
      if (isAllSelectedDutiesAreCMAEligible) {
        if (reimbursementMethod === ReimbursementMethod.CurrentMonthAdjustment)
          Right(
            new OverpaymentsScheduledJourney(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = None
              )
            )
          )
        else
          Right(
            new OverpaymentsScheduledJourney(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = computeBankAccountDetails
              )
            )
          )
      } else
        Left("submitReimbursementMethod.notCMAEligible")
    }

  def resetReimbursementMethod(): OverpaymentsScheduledJourney =
    whileClaimIsAmendable {
      new OverpaymentsScheduledJourney(
        answers.copy(
          reimbursementMethod = None,
          bankAccountType = None,
          bankAccountDetails = None
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

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[OverpaymentsScheduledJourney]) {
      val that = obj.asInstanceOf[OverpaymentsScheduledJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"OverpaymentsSingleJourney($answers,$caseNumber)"

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
          supportingEvidences     = answers.supportingEvidences
          scheduledDocument       = answers.scheduledDocument
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
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          reimbursementMethod = answers.reimbursementMethod.getOrElse(ReimbursementMethod.BankAccountTransfer),
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
                            reimbursementMethod: Option[ReimbursementMethod] = None,
                            selectedDocumentType: Option[UploadDocumentType] = None,
                            supportingEvidences: Seq[UploadedFile] = Seq.empty,
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

  object Checks extends OverpaymentsJourneyChecks[OverpaymentsScheduledJourney] {

    val reimbursementMethodHasBeenProvidedIfNeeded: Validate[OverpaymentsScheduledJourney] =
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
  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsScheduledJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      reimbursementMethodHasBeenProvidedIfNeeded,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided
    )

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
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .mapWhenDefined(answers.whetherNorthernIreland)(_.submitWhetherNorthernIreland)
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
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
