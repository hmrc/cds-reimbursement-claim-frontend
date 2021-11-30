/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DocumentTypeRejectedGoods
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentImplicits
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OptionsValidator._

import java.time.LocalDate

/** An encapsulated C&E1179 single MRN journey logic.
  * The constructor of this class MUST stay private to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsSingleJourney.Answers]] - keeps record of user anwers and acquired documents
  *  - [[RejectedGoodsSingleJourney.Outcome]] - final outcome of the journey to be sent to backend processing
  */
final class RejectedGoodsSingleJourney private (val answers: RejectedGoodsSingleJourney.Answers)
    extends FluentSyntax[RejectedGoodsSingleJourney] {

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def isComplete: Boolean =
    RejectedGoodsSingleJourney.validator.apply(this).isValid

  /** Check if all the selected duties have reimbursement amount provided. */
  def isCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(_.forall(_._2.isDefined))

  /** Check is all the upoaded documents has document type selected. */
  def isCompleteSupportingEvidences: Boolean =
    answers.supportingEvidences.exists(_.forall(_._2.isDefined))

  def getNdrcDetails: Option[List[NdrcDetails]] =
    answers.displayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    answers.displayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getSelectedDuties: Option[Seq[TaxCode]] =
    answers.reimbursementClaims.map(_.keys.toSeq)

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.reimbursementClaims
      .map(_.keySet.map(getNdrcDetailsFor).collect { case Some(d) => d })
      .exists(_.forall(_.cmaEligible.isDefined))

  /** Reset the journey with the new MRN
    * or keep existing journey if submitted the same MRN as before.
    */
  def submitMovementReferenceNumber(mrn: MRN): RejectedGoodsSingleJourney =
    answers.movementReferenceNumber match {
      case Some(existing) if existing === mrn => this
      case _                                  =>
        new RejectedGoodsSingleJourney(RejectedGoodsSingleJourney.Answers(movementReferenceNumber = Some(mrn)))
    }

  /** Set the ACC14 declaration and reset all reimbursementClaims */
  def submitDisplayDeclaration(displayDeclaration: DisplayDeclaration): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(displayDeclaration = Some(displayDeclaration), reimbursementClaims = None)
    )

  def submitImporterEoriNumber(importerEoriNumber: Eori): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(importerEoriNumber = Some(importerEoriNumber))
    )

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(declarantEoriNumber = Some(declarantEoriNumber))
    )

  def submitDeclarantType(declarantType: DeclarantTypeAnswer): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(declarantType = Some(declarantType))
    )

  def submitContactDetails(contactDetails: MrnContactDetails): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(contactDetails = Some(contactDetails))
    )

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(contactAddress = Some(contactAddress))
    )

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsSingleJourney =
    basisOfClaim match {
      case BasisOfRejectedGoodsClaim.SpecialCircumstances =>
        new RejectedGoodsSingleJourney(answers.copy(basisOfClaim = Some(basisOfClaim)))

      case _ =>
        new RejectedGoodsSingleJourney(
          answers.copy(
            basisOfClaim = Some(basisOfClaim),
            basisOfClaimSpecialCircumstances = None
          )
        )
    }

  def submitBasisOfClaimSpecialCircumstancesDetails(
    basisOfClaimSpecialCircumstancesDetails: String
  ): Either[String, RejectedGoodsSingleJourney] =
    answers.basisOfClaim match {
      case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
        Right(
          new RejectedGoodsSingleJourney(
            answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstancesDetails))
          )
        )

      case _ => Left("basisOfClaim.not_matching")
    }

  // overwrites basisOfClaim with SpecialCircumstances enum value
  def forceSubmitBasisOfClaimSpecialCircumstances(
    basisOfClaimSpecialCircumstances: String
  ): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(
        basisOfClaim = Some(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstances)
      )
    )

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(methodOfDisposal = Some(methodOfDisposal))
    )

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
    )

  def selectTaxCodeForReimbursement(taxCode: TaxCode): Either[String, RejectedGoodsSingleJourney] =
    answers.displayDeclaration match {
      case None => Left("selectTaxCodeForReimbursement.missingDisplayDeclaration")

      case Some(_) =>
        if (getNdrcDetailsFor(taxCode).isDefined) {
          val newReimbursementClaims = answers.reimbursementClaims match {
            case None                      => Map(taxCode -> None)
            case Some(reimbursementClaims) =>
              reimbursementClaims.get(taxCode) match {
                case None => reimbursementClaims + (taxCode -> None)
                case _    => reimbursementClaims
              }
          }
          Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
        } else
          Left("selectTaxCodeForReimbursement.taxCodeNotInACC14")
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, RejectedGoodsSingleJourney] =
    answers.displayDeclaration match {
      case None => Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")

      case Some(_) =>
        val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(_).isDefined)
        if (allTaxCodesExistInACC14) {
          val newReimbursementClaims = answers.reimbursementClaims match {
            case None                      =>
              taxCodes.map(taxCode => (taxCode -> None)).toMap

            case Some(reimbursementClaims) =>
              taxCodes.map { taxCode =>
                taxCode -> reimbursementClaims.get(taxCode).flatten
              }.toMap
          }
          Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
        } else
          Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
    }

  def isValidReimbursementAmount(reimbursementAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    reimbursementAmount > 0 && reimbursementAmount <= BigDecimal(ndrcDetails.amount)

  def submitAmountForReimbursement(
    taxCode: TaxCode,
    reimbursementAmount: BigDecimal
  ): Either[String, RejectedGoodsSingleJourney] =
    answers.displayDeclaration match {
      case None =>
        Left("submitAmountForReimbursement.missingDisplayDeclaration")

      case Some(_) =>
        getNdrcDetailsFor(taxCode) match {
          case None =>
            Left("submitAmountForReimbursement.taxCodeNotInACC14")

          case Some(ndrcDetails) if isValidReimbursementAmount(reimbursementAmount, ndrcDetails) =>
            val newReimbursementClaims = answers.reimbursementClaims match {
              case None                      => Map(taxCode -> Some(reimbursementAmount))
              case Some(reimbursementClaims) => reimbursementClaims + (taxCode -> Some(reimbursementAmount))
            }
            Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))

          case _ =>
            Left("submitAmountForReimbursement.invalidReimbursementAmount")
        }
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: LocalDate): Either[String, RejectedGoodsSingleJourney] =
    if (inspectionDate.isAfter(LocalDate.now()))
      Right(
        new RejectedGoodsSingleJourney(
          answers.copy(inspectionDate = Some(inspectionDate))
        )
      )
    else
      Left("submitInspectionDate.mustBeFutureDate")

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(inspectionAddress = Some(inspectionAddress))
    )

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(bankAccountDetails = Some(bankAccountDetails))
    )

  def submitBankAccountType(bankAccountType: BankAccountType): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(bankAccountType = Some(bankAccountType))
    )

  def submitReimbursementMethod(
    reimbursementMethodAnswer: ReimbursementMethodAnswer
  ): Either[String, RejectedGoodsSingleJourney] =
    if (
      reimbursementMethodAnswer === ReimbursementMethodAnswer.BankAccountTransfer ||
      isAllSelectedDutiesAreCMAEligible
    )
      Right(
        new RejectedGoodsSingleJourney(
          answers.copy(reimbursementMethod = Some(reimbursementMethodAnswer))
        )
      )
    else
      Left("submitReimbursementMethodAnswer.notCMAEligible")

  def submitUploadedDocument(uploadedDocument: UploadDocument): RejectedGoodsSingleJourney =
    answers.supportingEvidences match {
      case Some(supportingEvidences) if supportingEvidences.contains(uploadedDocument) =>
        this

      case Some(supportingEvidences) =>
        new RejectedGoodsSingleJourney(
          answers.copy(supportingEvidences = Some(supportingEvidences + (uploadedDocument -> None)))
        )

      case None =>
        new RejectedGoodsSingleJourney(answers.copy(supportingEvidences = Some(Map(uploadedDocument -> None))))
    }

  def submitDocumentType(
    uploadReference: UploadReference,
    documentType: DocumentTypeRejectedGoods
  ): Either[String, RejectedGoodsSingleJourney] =
    answers.supportingEvidences match {
      case None                      => Left("submitDocumentType.missingSupportingEvidences")
      case Some(supportingEvidences) =>
        supportingEvidences.find(_._1.uploadReference === uploadReference) match {
          case None                      => Left("submitDocumentType.upscanReferenceNotFound")
          case Some((uploadDocument, _)) =>
            Right(
              new RejectedGoodsSingleJourney(
                answers.copy(supportingEvidences = Some(supportingEvidences + (uploadDocument -> Some(documentType))))
              )
            )
        }
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[RejectedGoodsSingleJourney]) {
      obj.asInstanceOf[RejectedGoodsSingleJourney].answers === this.answers
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"RejectedGoodsSingleJourney(${answers.toString()})"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], RejectedGoodsSingleJourney.Output] =
    RejectedGoodsSingleJourney.validator
      .apply(this)
      .toEither
      .flatMap(_ =>
        answers match {
          case RejectedGoodsSingleJourney.Answers(
                Some(mrn),
                _,
                importerEoriNumber,
                declarantEoriNumber,
                Some(declarantType),
                contactDetails,
                contactAddress,
                Some(basisOfClaim),
                basisOfClaimSpecialCircumstances,
                Some(methodOfDisposal),
                Some(detailsOfRejectedGoods),
                Some(reimbursementClaims),
                Some(inspectionDate),
                Some(inspectionAddress),
                bankAccountDetails,
                bankAccountType,
                reimbursementMethod,
                Some(supportingEvidences)
              ) =>
            Right(
              RejectedGoodsSingleJourney.Output(
                movementReferenceNumber = mrn,
                declarantType = declarantType,
                basisOfClaim = basisOfClaim,
                methodOfDisposal = methodOfDisposal,
                detailsOfRejectedGoods = detailsOfRejectedGoods,
                inspectionDate = inspectionDate,
                inspectionAddress = inspectionAddress,
                reimbursementClaims = reimbursementClaims.mapValues(_.get),
                supportingEvidences = supportingEvidences.mapValues(_.get),
                basisOfClaimSpecialCircumstances = basisOfClaimSpecialCircumstances,
                reimbursementMethod = reimbursementMethod,
                importerAndDeclarantEoriNumber = for (a <- importerEoriNumber; b <- declarantEoriNumber) yield (a, b),
                contactDetailsAndAddress = for (a <- contactDetails; b <- contactAddress) yield (a, b),
                bankAccountDetailsAndType = for (a <- bankAccountDetails; b <- bankAccountType) yield (a, b)
              )
            )
          case _ =>
            Left(List("Unfortunately could not produce the output, please check if all answers are complete."))
        }
      )

}

object RejectedGoodsSingleJourney extends FluentImplicits[RejectedGoodsSingleJourney] {

  /** A starting point to build new instance of the journey. */
  val empty: RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(Answers())

  // All user answers captured during C&E1179 single MRN journey
  final case class Answers(
    movementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    importerEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    declarantType: Option[DeclarantTypeAnswer] = None, // is it required at all?
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    reimbursementClaims: Option[Map[TaxCode, Option[BigDecimal]]] = None,
    inspectionDate: Option[LocalDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    reimbursementMethod: Option[ReimbursementMethodAnswer] = None,
    supportingEvidences: Option[Map[UploadDocument, Option[DocumentTypeRejectedGoods]]] = None
  )

  // Final output of the journey.
  final case class Output(
    movementReferenceNumber: MRN,
    declarantType: DeclarantTypeAnswer,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    methodOfDisposal: MethodOfDisposal,
    detailsOfRejectedGoods: String,
    inspectionDate: LocalDate,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: Map[TaxCode, BigDecimal],
    supportingEvidences: Map[UploadDocument, DocumentTypeRejectedGoods],
    basisOfClaimSpecialCircumstances: Option[String],
    reimbursementMethod: Option[ReimbursementMethodAnswer],
    importerAndDeclarantEoriNumber: Option[(Eori, Eori)],
    contactDetailsAndAddress: Option[(MrnContactDetails, ContactAddress)],
    bankAccountDetailsAndType: Option[(BankAccountDetails, BankAccountType)]
  )

  import com.github.arturopala.validator.Validator._

  /** Validate if all answers has been provided and the journey is ready to finalize. */
  val validator: Validate[RejectedGoodsSingleJourney] =
    all(
      checkIsDefined(_.answers.movementReferenceNumber, "missing movementReferenceNumber"),
      checkIsDefined(_.answers.displayDeclaration, "missing displayDeclaration"),
      checkIsDefined(_.answers.declarantType, "missing declarantType"),
      checkIsDefined(_.answers.basisOfClaim, "missing basisOfClaim"),
      checkIsDefined(_.answers.detailsOfRejectedGoods, "missing detailsOfRejectedGoods"),
      checkIsDefined(_.answers.inspectionDate, "missing inspectionDate"),
      checkIsDefined(_.answers.inspectionAddress, "missing inspectionAddress"),
      checkIsDefined(_.answers.inspectionAddress, "missing inspectionAddress"),
      checkIsDefined(_.answers.methodOfDisposal, "missing inspectionAddress"),
      check(_.isCompleteReimbursementClaims, "incomplete methodOfDisposal"),
      check(_.isCompleteSupportingEvidences, "incomplete supportingEvidences"),
      check(
        journey => allOrNone(journey.answers.importerEoriNumber, journey.answers.declarantEoriNumber),
        "importerEoriNumber and declarantEoriNumber must be defined both or none"
      ),
      check(
        journey => allOrNone(journey.answers.contactDetails, journey.answers.contactAddress),
        "contactDetails and contactAddress must be defined both or none"
      ),
      check(
        journey => allOrNone(journey.answers.bankAccountDetails, journey.answers.bankAccountType),
        "bankAccountDetails and bankAccountType must be defined both or none"
      ),
      whenTrue[RejectedGoodsSingleJourney](
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances)
      )(
        checkIsDefined(
          _.answers.basisOfClaimSpecialCircumstances,
          "basisOfClaimSpecialCircumstances must be defined when basisOfClaim value is SpecialCircumstances"
        )
      ),
      whenTrue[RejectedGoodsSingleJourney](_.isAllSelectedDutiesAreCMAEligible)(
        checkIsDefined(
          _.answers.reimbursementMethod,
          "reimbursementMethodAnswer must be defined when all selected duties are CMA eligible"
        )
      )
    )

  object Answers {
    implicit lazy val mapFormat1: Format[Map[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatWithOptionalValue[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[Map[UploadDocument, Option[DocumentTypeRejectedGoods]]] =
      MapFormat.formatWithOptionalValue[UploadDocument, DocumentTypeRejectedGoods]

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.format[Answers]
  }

  object Output {

    implicit lazy val mapFormat1: Format[Map[TaxCode, BigDecimal]] =
      MapFormat.format[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[Map[UploadDocument, DocumentTypeRejectedGoods]] =
      MapFormat.format[UploadDocument, DocumentTypeRejectedGoods]

    implicit val equality: Eq[Output]   = Eq.fromUniversalEquals[Output]
    implicit val format: Format[Output] = Json.format[Output]
  }

  implicit val format: Format[RejectedGoodsSingleJourney] =
    Format(
      Reads(Answers.format.reads(_).map(answers => new RejectedGoodsSingleJourney(answers))),
      Writes(journey => Answers.format.writes(journey.answers))
    )

  implicit val equality: Eq[RejectedGoodsSingleJourney] =
    Eq.fromUniversalEquals[RejectedGoodsSingleJourney]

}
