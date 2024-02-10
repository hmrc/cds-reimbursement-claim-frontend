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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.CurrentMonthAdjustment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DirectFluentSyntax

import java.time.LocalDate

/** An encapsulated C&E1179 single MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsSingleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[RejectedGoodsSingleJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class RejectedGoodsSingleJourney private (
  val answers: RejectedGoodsSingleJourney.Answers,
  val caseNumber: Option[String] = None,
  val features: Option[RejectedGoodsSingleJourney.Features]
) extends JourneyBase
    with DirectFluentSyntax[RejectedGoodsSingleJourney]
    with RejectedGoodsJourneyProperties
    with HaveInspectionDetails
    with JourneyAnalytics {

  type Type = RejectedGoodsSingleJourney

  val self: RejectedGoodsSingleJourney = this

  val validate: Validator.Validate[RejectedGoodsSingleJourney] =
    RejectedGoodsSingleJourney.validator

  private def copy(
    newAnswers: RejectedGoodsSingleJourney.Answers
  ): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(newAnswers, caseNumber, features)

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(rc => rc.nonEmpty && rc.forall(_._2.isDefined))

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  def needsBanksAccountDetailsSubmission: Boolean =
    !isSubsidyOnlyJourney &&
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

  def getSelectedDuties: Option[Seq[TaxCode]] =
    answers.reimbursementClaims.map(_.keys.toSeq)

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.reimbursementClaims
      .map(_.keySet.map(getNdrcDetailsFor).collect { case Some(d) => d })
      .exists(_.forall(_.isCmaEligible))

  def isAllSelectedDutiesAreCMAEligible(amounts: RejectedGoodsSingleJourney.ReimbursementClaims): Boolean =
    amounts.keySet
      .map(getNdrcDetailsFor)
      .collect { case Some(d) => d }
      .forall(_.isCmaEligible)

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

  def hasCmaReimbursementMethod: Boolean =
    answers.reimbursementMethod.contains(CurrentMonthAdjustment)

  def withDutiesChangeMode(enabled: Boolean): RejectedGoodsSingleJourney =
    this.copy(answers.copy(dutiesChangeMode = enabled))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsSingleDocumentTypes)

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
              new RejectedGoodsSingleJourney(
                RejectedGoodsSingleJourney
                  .Answers(
                    userEoriNumber = answers.userEoriNumber,
                    movementReferenceNumber = Some(mrn),
                    displayDeclaration = Some(displayDeclaration),
                    eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly),
                    nonce = answers.nonce
                  ),
                features = features
              )
            )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsSingleJourney] =
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

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsSingleJourney] =
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

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsSingleJourney =
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
  ): Either[String, RejectedGoodsSingleJourney] =
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

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, RejectedGoodsSingleJourney] =
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

              Right(
                this.copy(
                  if (!isAllSelectedDutiesAreCMAEligible(newReimbursementClaims))
                    answers.copy(reimbursementClaims = Some(newReimbursementClaims), reimbursementMethod = None)
                  else
                    answers.copy(reimbursementClaims = Some(newReimbursementClaims))
                )
              )
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
  ): Either[String, RejectedGoodsSingleJourney] =
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
                Right(this.copy(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
              } else
                Left("submitAmountForReimbursement.taxCodeNotSelectedYet")

            case _ =>
              Left("submitAmountForReimbursement.invalidReimbursementAmount")
          }
      }
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, RejectedGoodsSingleJourney] =
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsSingleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          this.copy(
            answers.copy(bankAccountDetails =
              Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
            )
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsSingleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          this.copy(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitReimbursementMethod(
    reimbursementMethod: ReimbursementMethod
  ): Either[String, RejectedGoodsSingleJourney] =
    whileClaimIsAmendable {
      if (isAllSelectedDutiesAreCMAEligible) {
        if (reimbursementMethod === ReimbursementMethod.CurrentMonthAdjustment)
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = None
              )
            )
          )
        else
          Right(
            this.copy(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethod),
                bankAccountDetails = computeBankAccountDetails
              )
            )
          )
      } else
        Left("submitReimbursementMethod.notCMAEligible")
    }

  def resetReimbursementMethod(): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(
          reimbursementMethod = None,
          bankAccountType = None,
          bankAccountDetails = None
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsSingleJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          this.copy(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsSingleJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsSingleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new RejectedGoodsSingleJourney(answers = this.answers, caseNumber = Some(caseNumber), features = features)
            )
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[RejectedGoodsSingleJourney]) {
      val that = obj.asInstanceOf[RejectedGoodsSingleJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String =
    s"RejectedGoodsSingleJourney($answers, caseNumber=$caseNumber, features=$features)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[Seq[String], RejectedGoodsSingleJourney.Output] =
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
          supportingEvidences     = answers.supportingEvidences
          claimantInformation    <- getClaimantInformation
          payeeType              <- answers.payeeType
        } yield RejectedGoodsSingleJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          payeeType = payeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          methodOfDisposal = methodOfDisposal,
          detailsOfRejectedGoods = detailsOfRejectedGoods,
          inspectionDate = inspectionDate,
          inspectionAddress = inspectionAddress,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod =
            if (isSubsidyOnlyJourney) ReimbursementMethod.Subsidy
            else answers.reimbursementMethod.getOrElse(ReimbursementMethod.BankAccountTransfer),
          bankAccountDetails =
            if (isSubsidyOnlyJourney) None
            else answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsSingleJourney extends JourneyCompanion[RejectedGoodsSingleJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce), features = features)

  type ReimbursementClaims = Map[TaxCode, Option[BigDecimal]]

  final case class Features(
    shouldBlockSubsidies: Boolean,
    shouldAllowSubsidyOnlyPayments: Boolean
  ) extends SubsidiesFeatures

  // All user answers captured during C&E1179 single MRN journey
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
    reimbursementClaims: Option[ReimbursementClaims] = None,
    inspectionDate: Option[InspectionDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    reimbursementMethod: Option[ReimbursementMethod] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = false,
    dutiesChangeMode: Boolean = false
  ) extends RejectedGoodsAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    payeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    methodOfDisposal: MethodOfDisposal,
    detailsOfRejectedGoods: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: Map[TaxCode, BigDecimal],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._
  import JourneyValidationErrors._

  object Checks extends RejectedGoodsJourneyChecks[RejectedGoodsSingleJourney] {

    val reimbursementMethodHasBeenProvidedIfNeeded: Validate[RejectedGoodsSingleJourney] =
      all(
        whenTrue(
          j => j.isAllSelectedDutiesAreCMAEligible && !j.isSubsidyOnlyJourney,
          checkIsDefined(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_MUST_BE_DEFINED
          )
        ),
        whenTrue(
          j => !j.isAllSelectedDutiesAreCMAEligible && !j.isSubsidyOnlyJourney,
          checkIsEmpty(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED
          )
        ),
        whenTrue(
          _.isSubsidyOnlyJourney,
          checkIsEmpty(
            _.answers.reimbursementMethod,
            REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED
          )
        )
      )
  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[RejectedGoodsSingleJourney] =
    all(
      hasMRNAndDisplayDeclaration,
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
      whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments
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

  implicit val format: Format[RejectedGoodsSingleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "features").readNullable[Features])(new RejectedGoodsSingleJourney(_, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "features").writeNullable[Features])(journey =>
        (journey.answers, journey.caseNumber, journey.features)
      )
    )

  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, RejectedGoodsSingleJourney] =
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
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .flatMapWhenDefined(answers.basisOfClaimSpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails
      )
      .mapWhenDefined(answers.methodOfDisposal)(_.submitMethodOfDisposal)
      .mapWhenDefined(answers.detailsOfRejectedGoods)(_.submitDetailsOfRejectedGoods)
      .flatMapWhenDefined(answers.reimbursementClaims.map(_.keySet.toSeq))(
        _.selectAndReplaceTaxCodeSetForReimbursement
      )
      .flatMapEachWhenDefinedAndMappingDefined(answers.reimbursementClaims)(_.submitAmountForReimbursement)
      .mapWhenDefined(answers.inspectionDate)(_.submitInspectionDate)
      .mapWhenDefined(answers.inspectionAddress)(_.submitInspectionAddress)
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType _)
      .flatMapWhenDefined(answers.reimbursementMethod)(_.submitReimbursementMethod _)
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
