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

/** An encapsulated C&E1179 multiple MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsMultipleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[RejectedGoodsMultipleJourney.Outcome]] - final outcome of the journey to be sent to backend processing
  */
final class RejectedGoodsMultipleJourney private (
  val answers: RejectedGoodsMultipleJourney.Answers,
  val caseNumber: Option[String] = None
) extends FluentSyntax[RejectedGoodsMultipleJourney] {

  val ZERO: BigDecimal = BigDecimal("0")

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def hasCompleteAnswers: Boolean =
    RejectedGoodsMultipleJourney.validator.apply(this).isValid

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(mrc =>
      mrc.nonEmpty && mrc.forall { case (_, rc) => rc.nonEmpty && rc.forall(_._2.isDefined) }
    )

  def hasCompleteSupportingEvidences: Boolean =
    answers.checkYourAnswersChangeMode &&
      answers.supportingEvidences.forall(_.documentType.isDefined)

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumbers.flatMap(_.headOption)

  def getNthMovementReferenceNumber(index: Int): Option[MRN] =
    answers.movementReferenceNumbers.flatMap { mrns =>
      if (index >= 0 && index < mrns.size) Some(mrns(index))
      else None
    }

  def getIndexOfMovementReferenceNumber(mrn: MRN): Option[Int] =
    answers.movementReferenceNumbers.flatMap(_.zipWithIndex.find(_._1 === mrn).map(_._2))

  def countOfMovementReferenceNumbers: Int =
    answers.movementReferenceNumbers.map(_.size).getOrElse(0)

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    getLeadMovementReferenceNumber.flatMap(getDisplayDeclarationFor)

  def getNthDisplayDeclaration(index: Int): Option[DisplayDeclaration] =
    getNthMovementReferenceNumber(index).flatMap(getDisplayDeclarationFor)

  def getDisplayDeclarationFor(mrn: MRN): Option[DisplayDeclaration] =
    for {
      declarations <- answers.displayDeclarations
      declaration  <- declarations.find(_.getMRN === mrn)
    } yield declaration

  def getReimbursementClaimsFor(mrn: MRN): Option[Map[TaxCode, Option[BigDecimal]]] =
    answers.reimbursementClaims.flatMap(_.get(mrn))

  def getReimbursementClaims: Map[MRN, Map[TaxCode, BigDecimal]] =
    answers.reimbursementClaims
      .map(_.mapValues(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) }))
      .getOrElse(Map.empty)

  def getConsigneeEoriFromACC14: Option[Eori] =
    getLeadDisplayDeclaration.flatMap(_.getConsigneeEori)

  def getConsigneeContactDetailsFromACC14: Option[ContactDetails] =
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails).flatMap(_.contactDetails)

  def getDeclarantEoriFromACC14: Option[Eori] =
    getLeadDisplayDeclaration.map(_.getDeclarantEori)

  def getDeclarantContactDetailsFromACC14: Option[ContactDetails] =
    getLeadDisplayDeclaration.flatMap(_.getDeclarantDetails.contactDetails)

  def isConsigneePostCodeFromAcc14: Option[Boolean] =
    getLeadDisplayDeclaration.map(
      _.getConsigneeDetails.exists(_.establishmentAddress.postalCode.isEmpty)
    )

  def isDeclarantPostCodeFromAcc14: Option[Boolean] =
    getLeadDisplayDeclaration.map(_.getDeclarantDetails).map(_.establishmentAddress.postalCode.isEmpty)

  /** Check if ACC14 have declarant EORI or consignee EORI matching user's EORI */
  def needsDeclarantAndConsigneeEoriSubmission: Boolean =
    !(getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromACC14.contains(answers.userEoriNumber))

  def needsDeclarantAndConsigneePostCode: Boolean =
    !isConsigneePostCodeFromAcc14.getOrElse(false) && !isDeclarantPostCodeFromAcc14.getOrElse(false)

  def needsBanksAccountDetailsSubmission: Boolean =
    answers.reimbursementMethod.isEmpty ||
      answers.reimbursementMethod.contains(ReimbursementMethodAnswer.BankAccountTransfer)

  def needsSpecialCircumstancesBasisOfClaim: Boolean =
    answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances)

  def getNdrcDetailsFor(mrn: MRN): Option[List[NdrcDetails]] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsList)

  def getBankAccountDetails: Option[BankAccountDetails] =
    Stream(
      answers.bankAccountDetails,
      getLeadDisplayDeclaration.flatMap(_.displayResponseDetail.maskedBankDetails.flatMap(_.consigneeBankDetails)),
      getLeadDisplayDeclaration.flatMap(_.displayResponseDetail.maskedBankDetails.flatMap(_.declarantBankDetails))
    ).find(_.nonEmpty).flatten

  def getNdrcDetailsFor(mrn: MRN, taxCode: TaxCode): Option[NdrcDetails] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsFor(taxCode.value))

  def getAvailableDuties(mrn: MRN): Seq[(TaxCode, Boolean)] =
    getNdrcDetailsFor(mrn)
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

  def getSelectedDuties(mrn: MRN): Option[Seq[TaxCode]] =
    getReimbursementClaimsFor(mrn).map(_.keys.toSeq)

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.reimbursementClaims
      .map(_.flatMap { case (mrn, rc) =>
        rc.keySet
          .map(getNdrcDetailsFor(mrn, _))
          .collect { case Some(d) => d }
      })
      .exists(_.forall(_.isCmaEligible))

  def getNextNdrcDetailsToClaim(mrn: MRN): Option[NdrcDetails] =
    getReimbursementClaimsFor(mrn)
      .flatMap(
        _.collectFirst { case (taxCode, None) => taxCode }
          .flatMap(getNdrcDetailsFor(mrn, _))
      )

  def getTotalReimbursementAmountFor(mrn: MRN): Option[BigDecimal] =
    getReimbursementClaimsFor(mrn).map(_.map(_._2.getOrElse(ZERO)).sum)

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.values.flatMap(_.map(_._2)).sum

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
        case ClaimantType.Consignee => getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails)
        case ClaimantType.Declarant => getLeadDisplayDeclaration.map(_.getDeclarantDetails)
        case ClaimantType.User      => getLeadDisplayDeclaration.map(_.getDeclarantDetails)
      },
      contactDetails,
      contactAddress
    )

  def getInspectionAddressType: Option[InspectionAddressType] =
    answers.inspectionAddress.map(_.addressType)

  def computeContactDetails(retrievedUser: RetrievedUserType): Option[MrnContactDetails] = (
    answers.contactDetails,
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)),
    getLeadDisplayDeclaration.flatMap(_.getDeclarantDetails.contactDetails),
    retrievedUser
  ) match {
    case (details @ Some(_), _, _, _)                                                                       =>
      details
    case (_, Some(consigneeContactDetails), _, individual: Individual)
        if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(
        MrnContactDetails(
          consigneeContactDetails.contactName.getOrElse(""),
          consigneeContactDetails.emailAddress
            .fold(individual.email.getOrElse(Email("")))(address => Email(address)),
          consigneeContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case (_, None, _, individual: Individual) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(
        MrnContactDetails(
          individual.name.map(_.toFullName).getOrElse(""),
          individual.email.getOrElse(Email("")),
          None
        )
      )
    case (_, _, Some(declarantContactDetails), individual: Individual)                                      =>
      Some(
        MrnContactDetails(
          declarantContactDetails.contactName.getOrElse(""),
          declarantContactDetails.emailAddress
            .fold(individual.email.getOrElse(Email("")))(address => Email(address)),
          declarantContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case _                                                                                                  => None
  }

  def computeAddressDetails: Option[ContactAddress] = (
    answers.contactAddress,
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails),
    getLeadDisplayDeclaration.map(_.getDeclarantDetails)
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

  def whileJourneyIsAmendable(body: => RejectedGoodsMultipleJourney): RejectedGoodsMultipleJourney =
    if (isFinalized) this else body

  def whileJourneyIsAmendable(
    body: => Either[String, RejectedGoodsMultipleJourney]
  ): Either[String, RejectedGoodsMultipleJourney] =
    if (isFinalized) Left(RejectedGoodsMultipleJourney.ValidationErrors.JOURNEY_ALREADY_FINALIZED) else body

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, RejectedGoodsMultipleJourney] =
    submitMovementReferenceNumberAndDeclaration(0, mrn, displayDeclaration)

  def submitMovementReferenceNumberAndDeclaration(
    index: Int,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (index < 0)
        Left("submitMovementReferenceNumber.negativeIndex")
      else if (index > countOfMovementReferenceNumbers)
        Left("submitMovementReferenceNumber.invalidIndex")
      else if (mrn =!= displayDeclaration.getMRN)
        Left(
          s"submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
        )
      else
        getNthMovementReferenceNumber(index) match {
          // do nothing if MRN value and positions does not change, and declaration is the same
          case Some(existingMrn)
              if existingMrn === mrn &&
                getDisplayDeclarationFor(mrn).contains(displayDeclaration) =>
            Right(this)

          // change an existing MRN
          case Some(existingMrn) =>
            if (getIndexOfMovementReferenceNumber(mrn).exists(_ =!= index))
              Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
            else if (index === 0) {
              // lead MRN change resets all the journey
              Right(
                new RejectedGoodsMultipleJourney(
                  RejectedGoodsMultipleJourney
                    .Answers(
                      userEoriNumber = answers.userEoriNumber,
                      movementReferenceNumbers = Some(Seq(mrn)),
                      displayDeclarations = Some(Seq(displayDeclaration)),
                      nonce = answers.nonce
                    )
                )
              )
            } else {
              // change of an existing MRN removes related declaration and claims
              Right(
                new RejectedGoodsMultipleJourney(
                  answers.copy(
                    movementReferenceNumbers = answers.movementReferenceNumbers
                      .map(mrns => (mrns.take(index) :+ mrn) ++ mrns.drop(index + 1)),
                    displayDeclarations = answers.displayDeclarations.map(
                      _.filterNot(_.displayResponseDetail.declarationId === existingMrn.value) :+ displayDeclaration
                    ),
                    reimbursementClaims = answers.reimbursementClaims.map(_ - existingMrn + (mrn -> Map.empty))
                  )
                )
              )
            }

          // add new MRN
          case None              =>
            Right(
              new RejectedGoodsMultipleJourney(
                answers.copy(
                  movementReferenceNumbers = answers.movementReferenceNumbers.map(_ :+ mrn).orElse(Some(Seq(mrn))),
                  displayDeclarations =
                    answers.displayDeclarations.map(_ :+ displayDeclaration).orElse(Some(Seq(displayDeclaration))),
                  reimbursementClaims = answers.reimbursementClaims
                    .map(_ + (mrn -> Map.empty[TaxCode, Option[BigDecimal]]))
                    .orElse(Some(Map(mrn -> Map.empty[TaxCode, Option[BigDecimal]])))
                )
              )
            )
        }
    }

  def removeMovementReferenceNumberAndDisplayDeclaration(mrn: MRN): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      getIndexOfMovementReferenceNumber(mrn) match {
        case None        => Left("removeMovementReferenceNumberAndDisplayDeclaration.notFound")
        case Some(0)     => Left("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveLeadMRN")
        case Some(index) =>
          Right(
            new RejectedGoodsMultipleJourney(
              answers.copy(
                movementReferenceNumbers = answers.movementReferenceNumbers
                  .map(mrns => mrns.take(index) ++ mrns.drop(index + 1)),
                displayDeclarations = answers.displayDeclarations.map(
                  _.filterNot(_.displayResponseDetail.declarationId === mrn.value)
                ),
                reimbursementClaims = answers.reimbursementClaims.map(_ - mrn)
              )
            )
          )
      }
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new RejectedGoodsMultipleJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
            )
          )
        else
          Left(
            s"submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14"
          )
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
          Right(
            new RejectedGoodsMultipleJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
          )
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      basisOfClaim match {
        case BasisOfRejectedGoodsClaim.SpecialCircumstances =>
          new RejectedGoodsMultipleJourney(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case _ =>
          new RejectedGoodsMultipleJourney(
            answers.copy(
              basisOfClaim = Some(basisOfClaim),
              basisOfClaimSpecialCircumstances = None
            )
          )
      }
    }

  def submitBasisOfClaimSpecialCircumstancesDetails(
    basisOfClaimSpecialCircumstancesDetails: String
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      answers.basisOfClaim match {
        case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
          Right(
            new RejectedGoodsMultipleJourney(
              answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstancesDetails))
            )
          )
        case _                                                    => Left("basisOfClaim.not_matching")
      }
    }

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    mrn: MRN,
    taxCodes: Seq[TaxCode]
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      getDisplayDeclarationFor(mrn) match {
        case None =>
          Left(s"selectAndReplaceTaxCodeSetForReimbursement.missingDisplayDeclaration")

        case Some(_) =>
          if (taxCodes.isEmpty)
            Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(mrn, _).isDefined)
            if (allTaxCodesExistInACC14) {
              val newReimbursementClaims = getReimbursementClaimsFor(mrn) match {
                case None                      =>
                  taxCodes.map(taxCode => taxCode -> None).toMap

                case Some(reimbursementClaims) =>
                  taxCodes.map { taxCode =>
                    taxCode -> reimbursementClaims.get(taxCode).flatten
                  }.toMap
              }

              Right(
                new RejectedGoodsMultipleJourney(
                  answers.copy(reimbursementClaims =
                    answers.reimbursementClaims
                      .map(_ + (mrn -> newReimbursementClaims))
                      .orElse(Some(Map(mrn -> newReimbursementClaims)))
                  )
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
    mrn: MRN,
    taxCode: TaxCode,
    reimbursementAmount: BigDecimal
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      getDisplayDeclarationFor(mrn) match {
        case None =>
          Left(s"submitAmountForReimbursement.missingDisplayDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(mrn, taxCode) match {
            case None =>
              Left("submitAmountForReimbursement.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidReimbursementAmount(reimbursementAmount, ndrcDetails) =>
              if (getSelectedDuties(mrn).exists(_.contains(taxCode))) {
                val newReimbursementClaims = getReimbursementClaimsFor(mrn) match {
                  case None                      => Map(taxCode -> Some(reimbursementAmount))
                  case Some(reimbursementClaims) => reimbursementClaims + (taxCode -> Some(reimbursementAmount))
                }
                Right(
                  new RejectedGoodsMultipleJourney(
                    answers.copy(reimbursementClaims =
                      answers.reimbursementClaims
                        .map(_ + (mrn -> newReimbursementClaims))
                        .orElse(Some(Map(mrn -> newReimbursementClaims)))
                    )
                  )
                )
              } else
                Left("submitAmountForReimbursement.taxCodeNotSelectedYet")

            case _ =>
              Left("submitAmountForReimbursement.invalidReimbursementAmount")
          }
      }
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsMultipleJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsMultipleJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitReimbursementMethod(
    reimbursementMethodAnswer: ReimbursementMethodAnswer
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (isAllSelectedDutiesAreCMAEligible) {
        if (reimbursementMethodAnswer === ReimbursementMethodAnswer.CurrentMonthAdjustment)
          Right(
            new RejectedGoodsMultipleJourney(
              answers.copy(
                reimbursementMethod = Some(reimbursementMethodAnswer),
                bankAccountDetails = None,
                bankAccountType = None
              )
            )
          )
        else
          Right(
            new RejectedGoodsMultipleJourney(
              answers
                .copy(reimbursementMethod = Some(reimbursementMethodAnswer), bankAccountDetails = getBankAccountDetails)
            )
          )
      } else
        Left("submitReimbursementMethodAnswer.notCMAEligible")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          new RejectedGoodsMultipleJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsMultipleJourney =
    whileJourneyIsAmendable {
      new RejectedGoodsMultipleJourney(answers.copy(checkYourAnswersChangeMode = enabled))
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsMultipleJourney] =
    whileJourneyIsAmendable {
      RejectedGoodsMultipleJourney.validator
        .apply(this)
        .toEither
        .fold(
          errors => Left(errors.headOption.getOrElse("completeWith.invalidJourney")),
          _ => Right(new RejectedGoodsMultipleJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[RejectedGoodsMultipleJourney]) {
      val that = obj.asInstanceOf[RejectedGoodsMultipleJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"RejectedGoodsMultipleJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], RejectedGoodsMultipleJourney.Output] =
    RejectedGoodsMultipleJourney.validator
      .apply(this)
      .toEither
      .flatMap(_ =>
        (for {
          movementReferenceNumbers <- answers.movementReferenceNumbers
          basisOfClaim             <- answers.basisOfClaim
          methodOfDisposal         <- answers.methodOfDisposal
          detailsOfRejectedGoods   <- answers.detailsOfRejectedGoods
          inspectionDate           <- answers.inspectionDate
          inspectionAddress        <- answers.inspectionAddress
          supportingEvidences       = answers.supportingEvidences
          claimantInformation      <- getClaimantInformation
        } yield RejectedGoodsMultipleJourney.Output(
          movementReferenceNumbers = movementReferenceNumbers,
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

object RejectedGoodsMultipleJourney extends FluentImplicits[RejectedGoodsMultipleJourney] {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori): RejectedGoodsMultipleJourney =
    new RejectedGoodsMultipleJourney(Answers(userEoriNumber = userEoriNumber))

  type ReimbursementClaims = Map[TaxCode, Option[BigDecimal]]

  // All user answers captured during C&E1179 multiple MRNs journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumbers: Option[Seq[MRN]] = None,
    displayDeclarations: Option[Seq[DisplayDeclaration]] = None,
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    reimbursementClaims: Option[Map[MRN, ReimbursementClaims]] = None,
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
    movementReferenceNumbers: Seq[MRN],
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    methodOfDisposal: MethodOfDisposal,
    detailsOfRejectedGoods: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    reimbursementClaims: Map[MRN, Map[TaxCode, BigDecimal]],
    reimbursementMethod: ReimbursementMethodAnswer,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._
  import RejectedGoodsMultipleJourney.ValidationErrors._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  val validator: Validate[RejectedGoodsMultipleJourney] =
    all(
      check(_.answers.movementReferenceNumbers.exists(_.nonEmpty), MISSING_MOVEMENT_REFERENCE_NUMBER),
      check(_.answers.displayDeclarations.exists(_.nonEmpty), MISSING_DISPLAY_DECLARATION),
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

    implicit lazy val mapFormat3: Format[Map[MRN, Map[TaxCode, Option[BigDecimal]]]] =
      MapFormat.format[MRN, Map[TaxCode, Option[BigDecimal]]]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.format[Answers]
  }

  object Output {

    implicit lazy val mapFormat1: Format[Map[TaxCode, BigDecimal]] =
      MapFormat.format[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[Map[MRN, Map[TaxCode, BigDecimal]]] =
      MapFormat.format[MRN, Map[TaxCode, BigDecimal]]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Output]   = Eq.fromUniversalEquals[Output]
    implicit val format: Format[Output] = Json.format[Output]
  }

  import play.api.libs.functional.syntax._

  implicit val format: Format[RejectedGoodsMultipleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new RejectedGoodsMultipleJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  implicit val equality: Eq[RejectedGoodsMultipleJourney] =
    Eq.fromUniversalEquals[RejectedGoodsMultipleJourney]

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
    val BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED: String                 =
      "bankAccountDetailsMustNotBeDefined when reimbursementMethodAnswer is CurrentMonthAdjustment"
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