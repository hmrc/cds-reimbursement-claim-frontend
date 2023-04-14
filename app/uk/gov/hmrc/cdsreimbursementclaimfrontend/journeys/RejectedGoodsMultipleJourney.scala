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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils._

import java.time.LocalDate

/** An encapsulated C&E1179 multiple MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[RejectedGoodsMultipleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[RejectedGoodsMultipleJourney.Output]] - final outcome of the journey to be sent to backend processing
  */
final class RejectedGoodsMultipleJourney private (
  val answers: RejectedGoodsMultipleJourney.Answers,
  val caseNumber: Option[String] = None
) extends JourneyBase
    with DirectFluentSyntax[RejectedGoodsMultipleJourney]
    with RejectedGoodsJourneyProperties
    with CanSubmitMrnAndDeclaration
    with CanSubmitContactDetails
    with HaveInspectionDetails {

  type Type = RejectedGoodsMultipleJourney

  val self: RejectedGoodsMultipleJourney = this

  val validate: Validator.Validate[RejectedGoodsMultipleJourney] =
    RejectedGoodsMultipleJourney.validator

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.reimbursementClaims.exists(mrc =>
      mrc.size === countOfMovementReferenceNumbers && mrc.forall { case (_, rc) =>
        rc.nonEmpty && rc.forall(_._2.isDefined)
      }
    )

  def getMovementReferenceNumbers: Option[Seq[MRN]] =
    answers.movementReferenceNumbers

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

  def hasCompleteMovementReferenceNumbers: Boolean =
    countOfMovementReferenceNumbers >= 2

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

  def getReimbursementClaimFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    getReimbursementClaimsFor(mrn).flatMap(_.get(taxCode).flatten)

  def getReimbursementClaims: OrderedMap[MRN, Map[TaxCode, BigDecimal]] =
    answers.reimbursementClaims
      .map(
        _.view.view
          .mapValues(_.collect { case (taxCode, Some(amount)) => (taxCode, amount) })
          .to(OrderedMap)
      )
      .getOrElse(OrderedMap.empty)

  def needsBanksAccountDetailsSubmission: Boolean =
    true

  def needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex: Int): Boolean =
    if (pageIndex === 1) needsDeclarantAndConsigneeEoriSubmission else false

  def getNdrcDetailsFor(mrn: MRN): Option[List[NdrcDetails]] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(mrn: MRN, taxCode: TaxCode): Option[NdrcDetails] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsFor(taxCode.value))

  /** Returns the amount paid for the given MRN and tax code as returned by ACC14,
    * or None if either MRN or tax code not found.
    */
  def getAmountPaidFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    getNdrcDetailsFor(mrn, taxCode).map(_.amount).map(BigDecimal.apply)

  /** If the user has selected the tax code for repayment
    * then returns the amount paid for the given MRN and tax code as returned by ACC14,
    * otherwise None.
    */
  def getAmountPaidForIfSelected(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    getSelectedDuties(mrn)
      .flatMap(selectedTaxCodes =>
        if (selectedTaxCodes.contains(taxCode))
          getAmountPaidFor(mrn, taxCode)
        else None
      )

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
    getReimbursementClaimsFor(mrn).map(_.keys.toSeq.sorted)

  def getAllSelectedDuties: Seq[(MRN, Seq[TaxCode])] =
    answers.movementReferenceNumbers
      .map(_.map { mrn =>
        (mrn, getSelectedDuties(mrn).getOrElse(Seq.empty))
      })
      .getOrElse(Seq.empty)

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
    getReimbursementClaims.values.flatMap(_.values).sum

  def withDutiesChangeMode(enabled: Boolean): RejectedGoodsMultipleJourney =
    new RejectedGoodsMultipleJourney(answers.copy(dutiesChangeMode = enabled))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsMultipleDocumentTypes)

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ) =
    submitMovementReferenceNumberAndDeclaration(0, mrn, displayDeclaration)

  def submitMovementReferenceNumberAndDeclaration(
    index: Int,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ) =
    whileClaimIsAmendable {
      if (index < 0)
        Left("submitMovementReferenceNumber.negativeIndex")
      else if (index > countOfMovementReferenceNumbers)
        Left("submitMovementReferenceNumber.invalidIndex")
      else if (mrn =!= displayDeclaration.getMRN)
        Left(
          "submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
        )
      else if (
        index > 0 &&
        !getLeadDisplayDeclaration.exists(displayDeclaration.hasSameEoriAs)
      )
        Left(
          "submitMovementReferenceNumber.wrongDisplayDeclarationEori"
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
              // first MRN change resets all the journey
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
                    reimbursementClaims =
                      answers.reimbursementClaims.map(_.removed(existingMrn).updated(mrn, Map.empty))
                  )
                )
              )
            }

          // add new MRN
          case None              =>
            if (getIndexOfMovementReferenceNumber(mrn).isDefined)
              Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
            else
              Right(
                new RejectedGoodsMultipleJourney(
                  answers.copy(
                    movementReferenceNumbers = answers.movementReferenceNumbers.map(_ :+ mrn).orElse(Some(Seq(mrn))),
                    displayDeclarations =
                      answers.displayDeclarations.map(_ :+ displayDeclaration).orElse(Some(Seq(displayDeclaration))),
                    reimbursementClaims = answers.reimbursementClaims
                      .map(_ + (mrn -> Map.empty[TaxCode, Option[BigDecimal]]))
                      .orElse(Some(OrderedMap(mrn -> Map.empty[TaxCode, Option[BigDecimal]])))
                  )
                )
              )
        }
    }

  def removeMovementReferenceNumberAndDisplayDeclaration(mrn: MRN): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      getIndexOfMovementReferenceNumber(mrn) match {
        case None                                             => Left("removeMovementReferenceNumberAndDisplayDeclaration.notFound")
        case Some(0)                                          => Left("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveFirstMRN")
        case Some(1) if countOfMovementReferenceNumbers === 2 =>
          Left("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveSecondMRN")
        case Some(index)                                      =>
          Right(
            new RejectedGoodsMultipleJourney(
              answers.copy(
                movementReferenceNumbers = answers.movementReferenceNumbers
                  .map(mrns => mrns.take(index) ++ mrns.drop(index + 1)),
                displayDeclarations = answers.displayDeclarations.map(
                  _.filterNot(_.displayResponseDetail.declarationId === mrn.value)
                ),
                reimbursementClaims = answers.reimbursementClaims.map(_.removed(mrn))
              )
            )
          )
      }
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (
          getConsigneeEoriFromACC14 match {
            case Some(eori) => eori === consigneeEoriNumber
            case None       => getDeclarantEoriFromACC14.contains(consigneeEoriNumber)
          }
        )
          Right(
            new RejectedGoodsMultipleJourney(
              answers.copy(eoriNumbersVerification =
                answers.eoriNumbersVerification
                  .orElse(Some(EoriNumbersVerification()))
                  .map(_.copy(consigneeEoriNumber = Some(consigneeEoriNumber)))
              )
            )
          )
        else
          Left(
            "submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14"
          )
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
          Right(
            new RejectedGoodsMultipleJourney(
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
      new RejectedGoodsMultipleJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress) =
    whileClaimIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    mrn: MRN,
    taxCodes: Seq[TaxCode]
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      getDisplayDeclarationFor(mrn) match {
        case None =>
          Left("selectAndReplaceTaxCodeSetForReimbursement.missingDisplayDeclaration")

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
                      .orElse(Some(OrderedMap(mrn -> newReimbursementClaims)))
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
    whileClaimIsAmendable {
      getDisplayDeclarationFor(mrn) match {
        case None =>
          Left("submitAmountForReimbursement.missingDisplayDeclaration")

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
                        .orElse(Some(OrderedMap(mrn -> newReimbursementClaims)))
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
    whileClaimIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      new RejectedGoodsMultipleJourney(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsMultipleJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new RejectedGoodsMultipleJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      new RejectedGoodsMultipleJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
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
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => new RejectedGoodsMultipleJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
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
  def toOutput: Either[Seq[String], RejectedGoodsMultipleJourney.Output] =
    validate(this).left
      .map(_.messages)
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
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsMultipleJourney extends JourneyCompanion[RejectedGoodsMultipleJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): RejectedGoodsMultipleJourney =
    new RejectedGoodsMultipleJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type ReimbursementClaims = Map[TaxCode, Option[BigDecimal]]

  // All user answers captured during C&E1179 multiple MRNs journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumbers: Option[Seq[MRN]] = None,
    displayDeclarations: Option[Seq[DisplayDeclaration]] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    reimbursementClaims: Option[OrderedMap[MRN, ReimbursementClaims]] = None,
    inspectionDate: Option[InspectionDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = false,
    dutiesChangeMode: Boolean = false
  ) extends RejectedGoodsAnswers

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
    reimbursementClaims: OrderedMap[MRN, Map[TaxCode, BigDecimal]],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import JourneyValidationErrors._
  import com.github.arturopala.validator.Validator._

  object Checks extends RejectedGoodsJourneyChecks[RejectedGoodsMultipleJourney] {

    val hasMultipleMovementReferenceNumbers: Validate[RejectedGoodsMultipleJourney] =
      checkIsTrue(_.answers.movementReferenceNumbers.exists(_.size > 1), MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER)
  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[RejectedGoodsMultipleJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      hasMultipleMovementReferenceNumbers,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
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
  implicit val format: Format[RejectedGoodsMultipleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new RejectedGoodsMultipleJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  override def tryBuildFrom(answers: Answers): Either[String, RejectedGoodsMultipleJourney] =
    empty(answers.userEoriNumber, answers.nonce)
      .flatMapEachWhenDefined(answers.movementReferenceNumbers.zipOpt(answers.displayDeclarations).zipWithIndex)(j => {
        case ((mrn: MRN, decl: DisplayDeclaration), index: Int) =>
          j.submitMovementReferenceNumberAndDeclaration(index, mrn, decl)
      })
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
      .flatMapEachWhenDefined(answers.reimbursementClaims)(j => {
        case (mrn: MRN, reimbursements: Map[TaxCode, Option[BigDecimal]]) =>
          j.selectAndReplaceTaxCodeSetForReimbursement(mrn, reimbursements.keySet.toSeq)
            .flatMapEachWhenMappingDefined(reimbursements)(j =>
              (taxCode, amount) => j.submitAmountForReimbursement(mrn, taxCode, amount)
            )
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
