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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentImplicits
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

import java.time.LocalDate

/** An encapsulated C&E1179 single MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsSingleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[RejectedGoodsSingleJourney.Outcome]] - final outcome of the journey to be sent to backend processing
  */
final class RejectedGoodsSingleJourney private (
  val answers: RejectedGoodsSingleJourney.Answers,
  val caseNumber: Option[String] = None
) extends FluentSyntax[RejectedGoodsSingleJourney] {

  val ZERO: BigDecimal = BigDecimal("0")

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def hasCompleteAnswers: Boolean =
    RejectedGoodsSingleJourney.validator.apply(this).isValid

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(rc => rc.nonEmpty && rc.forall(_._2.isDefined))

  def hasCompleteSupportingEvidences: Boolean =
    answers.checkYourAnswersChangeMode &&
      answers.supportingEvidences.forall(_.documentType.isDefined)

  def getConsigneeEoriFromACC14: Option[Eori] =
    answers.displayDeclaration.flatMap(_.getConsigneeEori)

  def getConsigneeContactDetailsFromACC14: Option[ContactDetails] =
    answers.displayDeclaration.flatMap(_.getConsigneeDetails).flatMap(_.contactDetails)

  def getDeclarantEoriFromACC14: Option[Eori] =
    answers.displayDeclaration.map(_.getDeclarantEori)

  def getDeclarantContactDetailsFromACC14: Option[ContactDetails] =
    answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails)

  def isConsigneePostCodeFromAcc14: Option[Boolean] =
    answers.displayDeclaration.map(
      _.getConsigneeDetails.exists(_.establishmentAddress.postalCode.isEmpty)
    )

  def isDeclarantPostCodeFromAcc14: Option[Boolean] =
    answers.displayDeclaration.map(_.getDeclarantDetails).map(_.establishmentAddress.postalCode.isEmpty)

  /** Check if ACC14 have declarant EORI or consignee EORI matching user's EORI */
  def needsDeclarantAndConsigneeEoriSubmission: Boolean =
    !(getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromACC14.contains(answers.userEoriNumber))

  def needsDeclarantAndConsigneePostCode: Boolean =
    !isConsigneePostCodeFromAcc14.getOrElse(false) && !isDeclarantPostCodeFromAcc14.getOrElse(false)

  def needsBanksAccountDetailsAndTypeSubmission: Boolean =
    answers.reimbursementMethod.isEmpty ||
      answers.reimbursementMethod.contains(ReimbursementMethodAnswer.BankAccountTransfer)

  def needsSpecialCircumstancesBasisOfClaim: Boolean =
    answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances)

  def getNdrcDetails: Option[List[NdrcDetails]] =
    answers.displayDeclaration.flatMap(_.getNdrcDetailsList)

  def getBankAccountDetails: Option[BankAccountDetails] =
    Stream(
      answers.bankAccountDetails,
      answers.displayDeclaration.flatMap(_.displayResponseDetail.maskedBankDetails.flatMap(_.consigneeBankDetails)),
      answers.displayDeclaration.flatMap(_.displayResponseDetail.maskedBankDetails.flatMap(_.declarantBankDetails))
    ).find(_.nonEmpty).flatten

  def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
    answers.displayDeclaration.flatMap(_.getNdrcDetailsFor(taxCode.value))

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

  def getClaimantInformation: Option[ClaimantInformation] =
    for {
      contactDetails <- answers.contactDetails
      contactAddress <- answers.contactAddress
    } yield ClaimantInformation.from(
      getClaimantEori,
      getClaimantType match {
        case ClaimantType.Consignee => answers.displayDeclaration.flatMap(_.getConsigneeDetails)
        case ClaimantType.Declarant => answers.displayDeclaration.map(_.getDeclarantDetails)
        case ClaimantType.User      => answers.displayDeclaration.map(_.getDeclarantDetails)
      },
      contactDetails,
      contactAddress
    )

  def getInspectionAddressType: Option[InspectionAddressType] =
    answers.inspectionAddress.map(_.addressType)

  def computeContactDetails(retrievedUser: RetrievedUserType): Option[MrnContactDetails] = (
    answers.contactDetails,
    answers.displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)),
    answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails),
    retrievedUser
  ) match {
    case (details @ Some(_), _, _, _)                                                                           =>
      details
    case (_, Some(consigneeContactDetails), _, individual: Individual) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(
        MrnContactDetails(
          consigneeContactDetails.contactName.getOrElse(""),
          consigneeContactDetails.emailAddress
            .fold(individual.email.getOrElse(Email("")))
            (address => Email(address)),
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
    case (_, _, Some(declarantContactDetails), individual: Individual)                                                               =>
      Some(
        MrnContactDetails(
          declarantContactDetails.contactName.getOrElse(""),
          declarantContactDetails.emailAddress
            .fold(individual.email.getOrElse(Email("")))
            (address=>Email(address)),
          declarantContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case _                                                                                                      => None
  }

  def computeAddressDetails: Option[ContactAddress] = (
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

  def isFinalized: Boolean = caseNumber.isDefined

  def whileJourneyIsAmendable(body: => RejectedGoodsSingleJourney): RejectedGoodsSingleJourney =
    if (isFinalized) this else body

  def whileJourneyIsAmendable(
    body: => Either[String, RejectedGoodsSingleJourney]
  ): Either[String, RejectedGoodsSingleJourney] =
    if (isFinalized) Left(RejectedGoodsSingleJourney.ValidationErrors.JOURNEY_ALREADY_FINALIZED) else body

  /** Reset the journey with the new MRN
    * or keep existing journey if submitted the same MRN as before.
    */
  def submitMovementReferenceNumber(mrn: MRN): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      answers.movementReferenceNumber match {
        case Some(existing) if existing === mrn => this
        case _                                  =>
          new RejectedGoodsSingleJourney(
            RejectedGoodsSingleJourney
              .Answers(
                userEoriNumber = answers.userEoriNumber,
                movementReferenceNumber = Some(mrn),
                nonce = answers.nonce
              )
          )
      }
    }

  /** Set the ACC14 declaration and reset all reimbursementClaims */
  def submitDisplayDeclaration(displayDeclaration: DisplayDeclaration): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(displayDeclaration = Some(displayDeclaration), reimbursementClaims = None)
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new RejectedGoodsSingleJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
            )
          )
        else Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
          Right(
            new RejectedGoodsSingleJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
          )
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
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
    }

  def submitBasisOfClaimSpecialCircumstancesDetails(
    basisOfClaimSpecialCircumstancesDetails: String
  ): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      answers.basisOfClaim match {
        case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
          Right(
            new RejectedGoodsSingleJourney(
              answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstancesDetails))
            )
          )
        case _                                                    => Left("basisOfClaim.not_matching")
      }
    }

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodes: Seq[TaxCode]): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      answers.displayDeclaration match {
        case None => Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")

        case Some(_) =>
          val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(_).isDefined)
          if (allTaxCodesExistInACC14) {
            val newReimbursementClaims = answers.reimbursementClaims match {
              case None                      =>
                taxCodes.map(taxCode => taxCode -> None).toMap

              case Some(reimbursementClaims) =>
                taxCodes.map { taxCode =>
                  taxCode -> reimbursementClaims.get(taxCode).flatten
                }.toMap
            }
            Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
          } else
            Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
      }
    }

  def isValidReimbursementAmount(reimbursementAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    reimbursementAmount > 0 && reimbursementAmount <= BigDecimal(ndrcDetails.amount)

  def submitAmountForReimbursement(
    taxCode: TaxCode,
    reimbursementAmount: BigDecimal
  ): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
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
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      if (needsBanksAccountDetailsAndTypeSubmission)
        Right(
          new RejectedGoodsSingleJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      if (needsBanksAccountDetailsAndTypeSubmission)
        Right(
          new RejectedGoodsSingleJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitReimbursementMethod(
    reimbursementMethodAnswer: ReimbursementMethodAnswer
  ): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      if (isAllSelectedDutiesAreCMAEligible)
        Right(
          new RejectedGoodsSingleJourney(
            answers.copy(reimbursementMethod = Some(reimbursementMethodAnswer))
          )
        )
      else
        Left("submitReimbursementMethodAnswer.notCMAEligible")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          new RejectedGoodsSingleJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsSingleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsSingleJourney(answers.copy(checkYourAnswersChangeMode = enabled))
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsSingleJourney] =
    whileJourneyIsAmendable {
      RejectedGoodsSingleJourney.validator
        .apply(this)
        .toEither
        .fold(
          errors => Left(errors.headOption.getOrElse("completeWith.invalidJourney")),
          _ => Right(new RejectedGoodsSingleJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[RejectedGoodsSingleJourney]) {
      val that = obj.asInstanceOf[RejectedGoodsSingleJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"RejectedGoodsSingleJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], RejectedGoodsSingleJourney.Output] =
    RejectedGoodsSingleJourney.validator
      .apply(this)
      .toEither
      .flatMap(_ =>
        (for {
          mrn                    <- answers.movementReferenceNumber
          basisOfClaim           <- answers.basisOfClaim
          methodOfDisposal       <- answers.methodOfDisposal
          detailsOfRejectedGoods <- answers.detailsOfRejectedGoods
          inspectionDate         <- answers.inspectionDate
          inspectionAddress      <- answers.inspectionAddress
          supportingEvidences     = answers.supportingEvidences
          claimantInformation    <- getClaimantInformation
        } yield RejectedGoodsSingleJourney.Output(
          movementReferenceNumber = mrn,
          claimantType = getClaimantType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          methodOfDisposal = methodOfDisposal,
          detailsOfRejectedGoods = detailsOfRejectedGoods,
          inspectionDate = inspectionDate,
          inspectionAddress = inspectionAddress,
          reimbursementClaims = getReimbursementClaims,
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod = answers.reimbursementMethod.getOrElse(ReimbursementMethodAnswer.BankAccountTransfer),
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

  def prettyPrint: String = Json.prettyPrint(Json.toJson(this))

}

object RejectedGoodsSingleJourney extends FluentImplicits[RejectedGoodsSingleJourney] {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori): RejectedGoodsSingleJourney =
    new RejectedGoodsSingleJourney(Answers(userEoriNumber = userEoriNumber))

  type ReimbursementClaims = Map[TaxCode, Option[BigDecimal]]

  // All user answers captured during C&E1179 single MRN journey
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
    reimbursementMethod: Option[ReimbursementMethodAnswer] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = false
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
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: Map[TaxCode, BigDecimal],
    reimbursementMethod: ReimbursementMethodAnswer,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._
  import RejectedGoodsSingleJourney.ValidationErrors._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  val validator: Validate[RejectedGoodsSingleJourney] =
    all(
      checkIsDefined(_.answers.movementReferenceNumber, MISSING_MOVEMENT_REFERENCE_NUMBER),
      checkIsDefined(_.answers.displayDeclaration, MISSING_DISPLAY_DECLARATION),
      checkIsDefined(_.answers.basisOfClaim, MISSING_BASIS_OF_CLAIM),
      checkIsDefined(_.answers.detailsOfRejectedGoods, MISSING_DETAILS_OF_REJECTED_GOODS),
      checkIsDefined(_.answers.inspectionDate, MISSING_INSPECTION_DATE),
      checkIsDefined(_.answers.inspectionAddress, MISSING_INSPECTION_ADDRESS),
      checkIsDefined(_.answers.methodOfDisposal, MISSING_METHOD_OF_DISPOSAL),
      check(_.hasCompleteReimbursementClaims, INCOMPLETE_REIMBURSEMENT_CLAIMS),
      check(_.hasCompleteSupportingEvidences, INCOMPLETE_SUPPORTING_EVIDENCES),
      checkIsDefined(_.answers.contactDetails, MISSING_CONTACT_DETAILS),
      checkIsDefined(_.answers.contactAddress, MISSING_CONTACT_ADDRESS),
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
        _.needsBanksAccountDetailsAndTypeSubmission,
        all(
          checkIsDefined(
            _.answers.bankAccountDetails,
            BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED
          ),
          checkIsDefined(
            _.answers.bankAccountType,
            BANK_ACCOUNT_TYPE_MUST_BE_DEFINED
          )
        )
      ),
      whenFalse(
        _.needsBanksAccountDetailsAndTypeSubmission,
        all(
          checkIsEmpty(
            _.answers.bankAccountDetails,
            BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED
          ),
          checkIsEmpty(
            _.answers.bankAccountType,
            BANK_ACCOUNT_TYPE_MUST_NOT_BE_DEFINED
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
      ),
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

  object Answers {
    implicit lazy val mapFormat1: Format[Map[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatWithOptionalValue[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[Map[UploadDocumentType, (Nonce, Seq[UploadedFile])]] =
      MapFormat.format[UploadDocumentType, (Nonce, Seq[UploadedFile])]

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

  import play.api.libs.functional.syntax._

  implicit val format: Format[RejectedGoodsSingleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new RejectedGoodsSingleJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  implicit val equality: Eq[RejectedGoodsSingleJourney] =
    Eq.fromUniversalEquals[RejectedGoodsSingleJourney]

  object ValidationErrors {
    val JOURNEY_ALREADY_FINALIZED: String                                = "journeyAlreadyFinalized"
    val MISSING_MOVEMENT_REFERENCE_NUMBER: String                        = "missingMovementReferenceNumber"
    val MISSING_DISPLAY_DECLARATION: String                              = "missingDisplayDeclaration"
    val MISSING_BASIS_OF_CLAIM: String                                   = "missingBasisOfClaim"
    val MISSING_DETAILS_OF_REJECTED_GOODS: String                        = "missingDetailsOfRejectedGoods"
    val MISSING_INSPECTION_DATE: String                                  = "missingInspectionDate"
    val MISSING_INSPECTION_ADDRESS: String                               = "missingInspectionAddress"
    val MISSING_METHOD_OF_DISPOSAL: String                               = "missingMethodOfDisposal"
    val INCOMPLETE_REIMBURSEMENT_CLAIMS: String                          = "incompleteReimbursementClaims"
    val INCOMPLETE_SUPPORTING_EVIDENCES: String                          = "incompleteSupportingEvidences"
    val MISSING_CONTACT_DETAILS: String                                  = "missingContactDetails"
    val MISSING_CONTACT_ADDRESS: String                                  = "missingContactAddress"
    val TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO: String     = "totalReimbursementAmountMustBeGreaterThanZero"
    val DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED: String                   =
      "declarantEoriNumberMustBeProvided if user's EORI is not matching those of ACC14 declarant or consignee"
    val DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14: String     = "declarantEoriNumberMustBeEqualToThatOfACC14"
    val CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED: String                   =
      "consigneeEoriNumberMustBeProvided if user's EORI is not matching those of ACC14 declarant or consignee"
    val CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14: String     = "consigneeEoriNumberMustBeEqualToThatOfACC14"
    val DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED: String       =
      "declarantEoriNumberDoesNotHaveToBeProvided if user's EORI is matching those of ACC14 declarant or consignee"
    val CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED: String       =
      "consigneeEoriNumberDoesNotHaveToBeProvided if user's EORI is matching those of ACC14 declarant or consignee"
    val BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED: String                     =
      "bankAccountDetailsMustBeDefined when reimbursementMethodAnswer is empty or not CurrentMonthAdjustment"
    val BANK_ACCOUNT_TYPE_MUST_BE_DEFINED: String                        =
      "bankAccountTypeMustBeDefined when reimbursementMethodAnswer is empty or not CurrentMonthAdjustment"
    val BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED: String                 =
      "bankAccountDetailsMustNotBeDefined when reimbursementMethodAnswer is CurrentMonthAdjustment"
    val BANK_ACCOUNT_TYPE_MUST_NOT_BE_DEFINED: String                    =
      "bankAccountTypeMustNotBeDefined when reimbursementMethodAnswer is CurrentMonthAdjustment"
    val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED: String     =
      "basisOfClaimSpecialCircumstancesMustBeDefined when basisOfClaim value is SpecialCircumstances"
    val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED: String =
      "basisOfClaimSpecialCircumstancesMustNotBeDefined when basisOfClaim value is not SpecialCircumstances"
    val REIMBURSEMENT_METHOD_MUST_BE_DEFINED: String                     =
      "reimbursementMethodMustBeDefined when all selected duties are CMA eligible"
    val REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED: String          =
      "reimbursementMethodAnswerMustNotBeDefined when not all of selected duties are CMA eligible"
  }

}
