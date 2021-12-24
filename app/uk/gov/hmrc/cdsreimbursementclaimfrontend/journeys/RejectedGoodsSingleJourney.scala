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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetails, BankAccountType, BasisOfRejectedGoodsClaim, ClaimantInformation, EvidenceDocument, InspectionAddress, InspectionAddressType, MethodOfDisposal, MrnContactDetails, RetrievedUserType, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
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

import java.time.LocalDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

/** An encapsulated C&E1179 single MRN journey logic.
  * The constructor of this class MUST stay private to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsSingleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[RejectedGoodsSingleJourney.Outcome]] - final outcome of the journey to be sent to backend processing
  */
final class RejectedGoodsSingleJourney private (val answers: RejectedGoodsSingleJourney.Answers)
    extends FluentSyntax[RejectedGoodsSingleJourney] {

  val ZERO: BigDecimal = BigDecimal("0")

  def getContactDetails(retrievedUser: RetrievedUserType): Option[MrnContactDetails] = (
    answers.contactDetails,
    answers.displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)),
    answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails),
    retrievedUser
  ) match {
    case (details @ Some(_), _, _, _)                                                                           =>
      details
    case (_, Some(consigneeContactDetails), _, _) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(
        MrnContactDetails(
          consigneeContactDetails.contactName.getOrElse(""),
          Email(consigneeContactDetails.emailAddress.getOrElse("")),
          consigneeContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case (_, None, _, individual: Individual) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber)     =>
      Some(
        MrnContactDetails(
          individual.name.map(_.toFullName).getOrElse(""),
          individual.email.getOrElse(Email("")),
          None
        )
      )
    case (_, _, Some(declarantContactDetails), _)                                                               =>
      Some(
        MrnContactDetails(
          declarantContactDetails.contactName.getOrElse(""),
          Email(declarantContactDetails.emailAddress.getOrElse("")),
          declarantContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case _                                                                                                      => None
  }

  def getAddressDetails: Option[ContactAddress] = (
    answers.contactAddress,
    answers.displayDeclaration.flatMap(_.getConsigneeDetails),
    answers.displayDeclaration.map(_.getDeclarantDetails)
  ) match {
    case (contactAddress @ Some(_), _, _)                                                                =>
      contactAddress
    case (None, Some(consigneeDetails), _) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(consigneeDetails.establishmentAddress.toContactAddress)
    case (None, _, Some(declarantDetails))                                                               =>
      Some(declarantDetails.establishmentAddress.toContactAddress)
    case _                                                                                               => None
  }

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def isComplete: Boolean =
    RejectedGoodsSingleJourney.validator.apply(this).isValid

  /** Check if all the selected duties have reimbursement amount provided. */
  def isCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(_.forall(_._2.isDefined))

  /** Check is all the upoaded documents has document type selected. */
  def isCompleteSupportingEvidences: Boolean =
    answers.supportingEvidences.exists(_.forall(_.documentType.isDefined))

  def getConsigneeEoriFromACC14: Option[Eori] =
    answers.displayDeclaration.flatMap(_.getConsigneeEori)

  def getDeclarantEoriFromACC14: Option[Eori] =
    answers.displayDeclaration.map(_.getDeclarantEori)

  /** Check if ACC14 have declarant EORI or consignee EORI matching user's EORI */
  def needsDeclarantAndConsigneeEoriSubmission: Boolean =
    !(getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromACC14.contains(answers.userEoriNumber))

  def needsBanksAccountDetailsAndTypeSubmission: Boolean =
    answers.reimbursementMethod.isEmpty ||
      answers.reimbursementMethod.contains(ReimbursementMethodAnswer.BankAccountTransfer)

  def needsSpecialCircumstancesBasisOfClaim: Boolean =
    answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances)

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

  def getReimbursementClaims: Map[TaxCode, BigDecimal] =
    answers.reimbursementClaims
      .map(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) })
      .getOrElse(Map.empty)

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.toSeq.map(_._2).sum

  def getClaimantType: ClaimantType =
    if (getConsigneeEoriFromACC14.contains(answers.userEoriNumber))
      ClaimantType.Consignee
    else if (getDeclarantEoriFromACC14.contains(answers.userEoriNumber))
      ClaimantType.Declarant
    else
      ClaimantType.User

  def getClaimantEori: Eori = getClaimantType match {
    case ClaimantType.Consignee => getConsigneeEoriFromACC14.getOrElse(answers.userEoriNumber)
    case ClaimantType.Declarant => getDeclarantEoriFromACC14.getOrElse(answers.userEoriNumber)
    case ClaimantType.User      => answers.userEoriNumber
  }

  /** Reset the journey with the new MRN
    * or keep existing journey if submitted the same MRN as before.
    */
  def submitMovementReferenceNumber(mrn: MRN): RejectedGoodsSingleJourney =
    answers.movementReferenceNumber match {
      case Some(existing) if existing === mrn => this
      case _                                  =>
        new RejectedGoodsSingleJourney(
          RejectedGoodsSingleJourney
            .Answers(
              userEoriNumber = answers.userEoriNumber,
              movementReferenceNumber = Some(mrn)
            )
        )
    }

  /** Set the ACC14 declaration and reset all reimbursementClaims */
  def submitDisplayDeclaration(displayDeclaration: DisplayDeclaration): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(displayDeclaration = Some(displayDeclaration), reimbursementClaims = None)
    )

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsSingleJourney] =
    if (needsDeclarantAndConsigneeEoriSubmission)
      if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
        Right(
          new RejectedGoodsSingleJourney(
            answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
          )
        )
      else Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
    else Left("submitConsigneeEoriNumber.unexpected")

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsSingleJourney] =
    if (needsDeclarantAndConsigneeEoriSubmission)
      if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
        Right(
          new RejectedGoodsSingleJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
        )
      else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
    else Left("submitDeclarantEoriNumber.unexpected")

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

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(methodOfDisposal = Some(methodOfDisposal))
    )

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
    )

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
            if (getSelectedDuties.exists(_.contains(taxCode))) {
              val newReimbursementClaims = answers.reimbursementClaims match {
                case None                      => Map(taxCode -> Some(reimbursementAmount))
                case Some(reimbursementClaims) => reimbursementClaims + (taxCode -> Some(reimbursementAmount))
              }
              Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
            } else
              Left("submitAmountForReimbursement.taxCodeNotSelectedYet")

          case _ =>
            Left("submitAmountForReimbursement.invalidReimbursementAmount")
        }
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: LocalDate): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(inspectionDate = Some(inspectionDate))
    )

  def submitInspectionAddressType(inspectionAddressType: InspectionAddressType): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(inspectionAddressType = Some(inspectionAddressType))
    )

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(
      answers.copy(inspectionAddress = Some(inspectionAddress))
    )

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsSingleJourney] =
    if (needsBanksAccountDetailsAndTypeSubmission)
      Right(
        new RejectedGoodsSingleJourney(
          answers.copy(bankAccountDetails = Some(bankAccountDetails))
        )
      )
    else Left("submitBankAccountDetails.unexpected")

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsSingleJourney] =
    if (needsBanksAccountDetailsAndTypeSubmission)
      Right(
        new RejectedGoodsSingleJourney(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    else Left("submitBankAccountType.unexpected")

  def submitReimbursementMethod(
    reimbursementMethodAnswer: ReimbursementMethodAnswer
  ): Either[String, RejectedGoodsSingleJourney] =
    if (isAllSelectedDutiesAreCMAEligible)
      Right(
        new RejectedGoodsSingleJourney(
          answers.copy(reimbursementMethod = Some(reimbursementMethodAnswer))
        )
      )
    else
      Left("submitReimbursementMethodAnswer.notCMAEligible")

  def submitUploadedDocument(uploadedDocument: UploadDocument): RejectedGoodsSingleJourney =
    answers.supportingEvidences match {
      case Some(supportingEvidences)
          if supportingEvidences.exists(_.uploadReference === uploadedDocument.uploadReference) =>
        new RejectedGoodsSingleJourney(
          answers.copy(supportingEvidences = Some(supportingEvidences.map {
            case d if d.uploadReference == uploadedDocument.uploadReference => uploadedDocument
            case d                                                          => d
          }))
        )

      case Some(supportingEvidences) =>
        new RejectedGoodsSingleJourney(
          answers.copy(supportingEvidences = Some(supportingEvidences :+ uploadedDocument))
        )

      case None =>
        new RejectedGoodsSingleJourney(answers.copy(supportingEvidences = Some(Seq(uploadedDocument))))
    }

  def submitDocumentType(
    uploadReference: UploadReference,
    documentType: UploadDocumentType
  ): Either[String, RejectedGoodsSingleJourney] =
    answers.supportingEvidences match {
      case None                      => Left("submitDocumentType.missingSupportingEvidences")
      case Some(supportingEvidences) =>
        supportingEvidences.find(_.uploadReference === uploadReference) match {
          case None    => Left("submitDocumentType.upscanReferenceNotFound")
          case Some(_) =>
            Right(
              new RejectedGoodsSingleJourney(
                answers.copy(supportingEvidences = Some(supportingEvidences.map {
                  case d if d.uploadReference == uploadReference => d.copy(documentType = Some(documentType))
                  case d                                         => d
                }))
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
                _,
                Some(mrn),
                _,
                _,
                _,
                Some(contactDetails),
                Some(contactAddress),
                Some(basisOfClaim),
                basisOfClaimSpecialCircumstances,
                Some(methodOfDisposal),
                Some(detailsOfRejectedGoods),
                _,
                Some(inspectionDate),
                Some(inspectionAddressType),
                Some(inspectionAddress),
                bankAccountDetails,
                _,
                reimbursementMethod,
                Some(supportingEvidences)
              ) =>
            Right(
              RejectedGoodsSingleJourney.Output(
                movementReferenceNumber = mrn,
                claimantType = getClaimantType,
                claimantInformation = ClaimantInformation
                  .from(
                    getClaimantEori,
                    getClaimantType match {
                      case ClaimantType.Consignee => answers.displayDeclaration.flatMap(_.getConsigneeDetails)
                      case ClaimantType.Declarant => answers.displayDeclaration.map(_.getDeclarantDetails)
                      case ClaimantType.User      => answers.displayDeclaration.map(_.getDeclarantDetails)
                    },
                    contactDetails,
                    contactAddress
                  ),
                basisOfClaim = basisOfClaim,
                methodOfDisposal = methodOfDisposal,
                detailsOfRejectedGoods = detailsOfRejectedGoods,
                inspectionDate = inspectionDate,
                inspectionAddress = inspectionAddress,
                inspectionAddressType = inspectionAddressType,
                reimbursementClaims = getReimbursementClaims,
                supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
                basisOfClaimSpecialCircumstances = basisOfClaimSpecialCircumstances,
                reimbursementMethod = reimbursementMethod.getOrElse(ReimbursementMethodAnswer.BankAccountTransfer),
                bankAccountDetails = bankAccountDetails
              )
            )
          case _ =>
            Left(List("Unfortunately could not produce the output, please check if all answers are complete."))
        }
      )

}

object RejectedGoodsSingleJourney extends FluentImplicits[RejectedGoodsSingleJourney] {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(Answers(userEoriNumber))

  // All user answers captured during C&E1179 single MRN journey
  final case class Answers(
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
    reimbursementClaims: Option[Map[TaxCode, Option[BigDecimal]]] = None,
    inspectionDate: Option[LocalDate] = None,
    inspectionAddressType: Option[InspectionAddressType] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    reimbursementMethod: Option[ReimbursementMethodAnswer] = None,
    supportingEvidences: Option[Seq[UploadDocument]] = None
  )

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    methodOfDisposal: MethodOfDisposal,
    detailsOfRejectedGoods: String,
    inspectionDate: LocalDate,
    inspectionAddressType: InspectionAddressType,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: Map[TaxCode, BigDecimal],
    reimbursementMethod: ReimbursementMethodAnswer,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  val validator: Validate[RejectedGoodsSingleJourney] =
    all(
      checkIsDefined(_.answers.movementReferenceNumber, "missing movementReferenceNumber"),
      checkIsDefined(_.answers.displayDeclaration, "missing displayDeclaration"),
      checkIsDefined(_.answers.basisOfClaim, "missing basisOfClaim"),
      checkIsDefined(_.answers.detailsOfRejectedGoods, "missing detailsOfRejectedGoods"),
      checkIsDefined(_.answers.inspectionDate, "missing inspectionDate"),
      checkIsDefined(_.answers.inspectionAddressType, "missing inspectionAddressType"),
      checkIsDefined(_.answers.inspectionAddress, "missing inspectionAddress"),
      checkIsDefined(_.answers.methodOfDisposal, "missing inspectionAddress"),
      check(_.isCompleteReimbursementClaims, "incomplete reimbursement claims"),
      check(_.isCompleteSupportingEvidences, "incomplete supporting evidences"),
      checkIsDefined(_.answers.contactDetails, "missing contactDetails"),
      checkIsDefined(_.answers.contactAddress, "missing contactAddress"),
      check(_.getTotalReimbursementAmount > 0, "total reimbursement amout must be greater than zero"),
      whenTrue(
        _.needsDeclarantAndConsigneeEoriSubmission,
        all(
          checkIsDefined(
            _.answers.declarantEoriNumber,
            "declarantEoriNumber must be provided if user's EORI is not matching those of ACC14 declarant or consignee"
          ),
          checkEquals(
            _.getDeclarantEoriFromACC14,
            _.answers.declarantEoriNumber,
            "declarantEoriNumber must be equal to that of ACC14"
          ),
          checkIsDefined(
            _.answers.consigneeEoriNumber,
            "consigneeEoriNumber must be provided if user's EORI is not matching those of ACC14 declarant or consignee"
          ),
          checkEquals(
            _.getConsigneeEoriFromACC14,
            _.answers.consigneeEoriNumber,
            "declarantEoriNumber must be equal to that of ACC14"
          )
        )
      ),
      whenFalse(
        _.needsDeclarantAndConsigneeEoriSubmission,
        all(
          checkIsEmpty(
            _.answers.declarantEoriNumber,
            "declarantEoriNumber does not have to be provided if user's EORI is matching those of ACC14 declarant or consignee"
          ),
          checkIsEmpty(
            _.answers.consigneeEoriNumber,
            "consigneeEoriNumber does not have to be provided if user's EORI is matching those of ACC14 declarant or consignee"
          )
        )
      ),
      whenTrue(
        _.needsBanksAccountDetailsAndTypeSubmission,
        all(
          checkIsDefined(
            _.answers.bankAccountDetails,
            "bankAccountDetails must be defined when reimbursementMethodAnswer is empty or not CurrentMonthAdjustment"
          ),
          checkIsDefined(
            _.answers.bankAccountType,
            "bankAccountType must be defined when reimbursementMethodAnswer is empty or not CurrentMonthAdjustment"
          )
        )
      ),
      whenFalse(
        _.needsBanksAccountDetailsAndTypeSubmission,
        all(
          checkIsEmpty(
            _.answers.bankAccountDetails,
            "bankAccountDetails must NOT be defined when reimbursementMethodAnswer is CurrentMonthAdjustment"
          ),
          checkIsEmpty(
            _.answers.bankAccountType,
            "bankAccountType must NOT be defined when reimbursementMethodAnswer is CurrentMonthAdjustment"
          )
        )
      ),
      whenTrue(
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        checkIsDefined(
          _.answers.basisOfClaimSpecialCircumstances,
          "basisOfClaimSpecialCircumstances must be defined when basisOfClaim value is SpecialCircumstances"
        )
      ),
      whenFalse(
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        checkIsEmpty(
          _.answers.basisOfClaimSpecialCircumstances,
          "basisOfClaimSpecialCircumstances must NOT be defined when basisOfClaim value is not SpecialCircumstances"
        )
      ),
      whenTrue(
        _.isAllSelectedDutiesAreCMAEligible,
        checkIsDefined(
          _.answers.reimbursementMethod,
          "reimbursementMethodAnswer must be defined when all selected duties are CMA eligible"
        )
      ),
      whenFalse(
        _.isAllSelectedDutiesAreCMAEligible,
        checkIsEmpty(
          _.answers.reimbursementMethod,
          "reimbursementMethodAnswer must NOT be defined when not all of selected duties are CMA eligible"
        )
      )
    )

  object Answers {
    implicit lazy val mapFormat1: Format[Map[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatWithOptionalValue[TaxCode, BigDecimal]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.format[Answers]
  }

  object Output {

    implicit lazy val mapFormat1: Format[Map[TaxCode, BigDecimal]] =
      MapFormat.format[TaxCode, BigDecimal]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

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
