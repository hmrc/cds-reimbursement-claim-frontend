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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils._

/** An encapsulated C&E1179 single MRN journey logic.
  * The constructor of this class MUST stay PRIVATE to protected integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *  - [[OverpaymentsMultipleJourney.Answers]] - keeps record of user answers and acquired documents
  *  - [[OverpaymentsMultipleJourney.Output]] - final output of the journey to be sent to backend processing
  */
final class OverpaymentsMultipleJourney private (
  val answers: OverpaymentsMultipleJourney.Answers,
  val caseNumber: Option[String] = None
) extends JourneyBase
    with DirectFluentSyntax[OverpaymentsMultipleJourney]
    with OverpaymentsJourneyProperties
    with CanSubmitMrnAndDeclaration {

  type Type = OverpaymentsMultipleJourney

  val self: OverpaymentsMultipleJourney = this

  val validate: Validator.Validate[OverpaymentsMultipleJourney] =
    OverpaymentsMultipleJourney.validator

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.correctedAmounts.exists(claims => claims.values.forall(rc => rc.nonEmpty && rc.forall(_._2.isDefined)))

  def getMovementReferenceNumbers: Option[Seq[MRN]] =
    answers.movementReferenceNumbers

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumbers.flatMap(_.headOption)

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclarations.flatMap(_.headOption)

  def getDisplayDeclarationFor(mrn: MRN): Option[DisplayDeclaration] =
    for {
      declarations <- answers.displayDeclarations
      declaration  <- declarations.find(_.getMRN === mrn)
    } yield declaration

  def getNthDisplayDeclaration(index: Int): Option[DisplayDeclaration] =
    getNthMovementReferenceNumber(index).flatMap(getDisplayDeclarationFor)

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

  def needsBanksAccountDetailsSubmission: Boolean =
    true

  def needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex: Int): Boolean =
    if (pageIndex === 1) needsDeclarantAndConsigneeEoriSubmission else false

  def needsDuplicateMrnAndDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry)

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadDisplayDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(mrn: MRN): Option[List[NdrcDetails]] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(declarationid: MRN, taxCode: TaxCode): Option[NdrcDetails] =
    getDisplayDeclarationFor(declarationid).flatMap(_.getNdrcDetailsFor(taxCode.value))

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

  def getCorrectedAmountFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    for {
      all           <- answers.correctedAmounts
      thisMrn       <- all.get(mrn)
      correctAmount <- thisMrn.get(taxCode).flatten
    } yield correctAmount

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

  def getSelectedDuties(declarationId: MRN): Option[Seq[TaxCode]] =
    answers.correctedAmounts.flatMap(_.get(declarationId)).map(_.keys.toSeq)

  def isAllSelectedDutiesAreCMAEligible(declarationId: MRN): Boolean =
    answers.correctedAmounts
      .flatMap(_.get(declarationId))
      .map(_.keySet.map(getNdrcDetailsFor(declarationId, _)).collect { case Some(d) => d })
      .exists(_.forall(_.isCmaEligible))

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.correctedAmounts.toList
      .flatMap(_.keys)
      .map(isAllSelectedDutiesAreCMAEligible)
      .contains(true)

  def getAvailableTaxCodesWithPaidAmountsFor(declarationId: MRN): Seq[(TaxCode, BigDecimal)] =
    getDisplayDeclarationFor(declarationId)
      .flatMap(_.getNdrcDutiesWithAmount)
      .getOrElse(Seq.empty)

  def getReimbursementClaimsFor(declarationId: MRN): Map[TaxCode, BigDecimal] = {
    val taxCodesWithPaidAmounts: Map[TaxCode, BigDecimal] =
      getAvailableTaxCodesWithPaidAmountsFor(declarationId).toMap

    answers.correctedAmounts
      .flatMap(
        _.get(declarationId)
      )
      .map(
        _.map { case (taxCode, correctAmountOpt) =>
          for {
            correctAmount <- correctAmountOpt
            paidAmount    <- taxCodesWithPaidAmounts.get(taxCode)
          } yield (taxCode, paidAmount - correctAmount)
        }.collect { case Some(x) => x }.toMap
      )
      .getOrElse(Map.empty)
  }

  def getReimbursementClaims: Map[MRN, Map[TaxCode, BigDecimal]] =
    answers.correctedAmounts
      .map(_.map { case (mrn, correctedAmountsPerMrn) =>
        val taxCodesWithPaidAmounts: Map[TaxCode, BigDecimal] =
          getAvailableTaxCodesWithPaidAmountsFor(mrn).toMap
        (
          mrn,
          correctedAmountsPerMrn
            .map { case (taxCode, correctAmountOpt) =>
              for {
                correctAmount <- correctAmountOpt
                paidAmount    <- taxCodesWithPaidAmounts.get(taxCode)
              } yield (taxCode, paidAmount - correctAmount)
            }
            .collect { case Some(x) => x }
            .toMap
        )
      })
      .getOrElse(Map.empty)

  def getNextNdrcDetailsToClaim(declarationId: MRN): Option[NdrcDetails] =
    answers.correctedAmounts
      .flatMap(_.get(declarationId))
      .flatMap(
        _.collectFirst { case (taxCode, None) => taxCode }
          .flatMap(getNdrcDetailsFor(declarationId, _))
      )

  def getReimbursementAmountForDeclaration(declarationId: MRN): BigDecimal =
    getReimbursementClaimsFor(declarationId).toSeq.map(_._2).sum

  def getTotalReimbursementAmount: BigDecimal =
    answers.correctedAmounts.map(_.keys.map(getReimbursementAmountForDeclaration).sum).getOrElse(ZERO)

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsMultipleJourney =
    new OverpaymentsMultipleJourney(answers.copy(dutiesChangeMode = enabled))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsSingleDocumentTypes)

  def getAvailableClaimTypes: BasisOfOverpaymentClaimsList =
    BasisOfOverpaymentClaimsList
      .withoutDuplicateEntry()
      .excludeNorthernIrelandClaims(answers.whetherNorthernIreland.getOrElse(false), getLeadDisplayDeclaration)

  /** Resets the journey with the new MRN
    * or keep existing journey if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    index: Int,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      if (index < 0)
        Left("submitMovementReferenceNumber.negativeIndex")
      else if (index > countOfMovementReferenceNumbers)
        Left("submitMovementReferenceNumber.invalidIndex")
      else if (mrn =!= displayDeclaration.getMRN)
        Left(
          s"submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
        )
      else if (
        index > 0 &&
        !getLeadDisplayDeclaration.exists(displayDeclaration.hasSameEoriAs)
      )
        Left(
          s"submitMovementReferenceNumber.wrongDisplayDeclarationEori"
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
                new OverpaymentsMultipleJourney(
                  OverpaymentsMultipleJourney
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
                new OverpaymentsMultipleJourney(
                  answers.copy(
                    movementReferenceNumbers = answers.movementReferenceNumbers
                      .map(mrns => (mrns.take(index) :+ mrn) ++ mrns.drop(index + 1)),
                    displayDeclarations = answers.displayDeclarations.map(
                      _.filterNot(_.displayResponseDetail.declarationId === existingMrn.value) :+ displayDeclaration
                    ),
                    correctedAmounts = answers.correctedAmounts.map(_ - existingMrn + (mrn -> Map.empty))
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
                new OverpaymentsMultipleJourney(
                  answers.copy(
                    movementReferenceNumbers = answers.movementReferenceNumbers.map(_ :+ mrn).orElse(Some(Seq(mrn))),
                    displayDeclarations =
                      answers.displayDeclarations.map(_ :+ displayDeclaration).orElse(Some(Seq(displayDeclaration))),
                    correctedAmounts = answers.correctedAmounts
                      .map(_ + (mrn -> Map.empty[TaxCode, Option[BigDecimal]]))
                      .orElse(Some(OrderedMap(mrn -> Map.empty[TaxCode, Option[BigDecimal]])))
                  )
                )
              )
        }
    }

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ) = submitMovementReferenceNumberAndDeclaration(0, mrn, displayDeclaration)

  def removeMovementReferenceNumberAndDisplayDeclaration(mrn: MRN): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      getIndexOfMovementReferenceNumber(mrn) match {
        case None                                             => Left("removeMovementReferenceNumberAndDisplayDeclaration.notFound")
        case Some(0)                                          => Left("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveFirstMRN")
        case Some(1) if countOfMovementReferenceNumbers === 2 =>
          Left("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveSecondMRN")
        case Some(index)                                      =>
          Right(
            new OverpaymentsMultipleJourney(
              answers.copy(
                movementReferenceNumbers = answers.movementReferenceNumbers
                  .map(mrns => mrns.take(index) ++ mrns.drop(index + 1)),
                displayDeclarations = answers.displayDeclarations.map(
                  _.filterNot(_.displayResponseDetail.declarationId === mrn.value)
                ),
                correctedAmounts = answers.correctedAmounts.map(_ - mrn)
              )
            )
          )
      }
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getConsigneeEoriFromACC14.contains(consigneeEoriNumber))
          Right(
            new OverpaymentsMultipleJourney(
              answers.copy(consigneeEoriNumber = Some(consigneeEoriNumber))
            )
          )
        else Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsDeclarantAndConsigneeEoriSubmission)
        if (getDeclarantEoriFromACC14.contains(declarantEoriNumber))
          Right(
            new OverpaymentsMultipleJourney(answers.copy(declarantEoriNumber = Some(declarantEoriNumber)))
          )
        else Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      new OverpaymentsMultipleJourney(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      new OverpaymentsMultipleJourney(
        answers.copy(contactAddress = Some(contactAddress))
      )
    }

  def submitWhetherNorthernIreland(whetherNorthernIreland: Boolean): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      new OverpaymentsMultipleJourney(
        answers.copy(
          whetherNorthernIreland = Some(whetherNorthernIreland),
          basisOfClaim =
            if (whetherNorthernIreland) answers.basisOfClaim
            else
              // review basis of claim if nothern ireland claims should not be allowed
              answers.basisOfClaim.flatMap { case basisOfClaim =>
                if (BasisOfOverpaymentClaimsList.northernIreland.contains(basisOfClaim)) None
                else Some(basisOfClaim)
              }
        )
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      basisOfClaim match {
        case BasisOfOverpaymentClaim.DuplicateEntry =>
          new OverpaymentsMultipleJourney(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case _ =>
          new OverpaymentsMultipleJourney(
            answers.copy(
              basisOfClaim = Some(basisOfClaim)
            )
          )
      }
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      new OverpaymentsMultipleJourney(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    declarationId: MRN,
    taxCodes: Seq[TaxCode]
  ): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      getDisplayDeclarationFor(declarationId) match {
        case None => Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")

        case Some(_) =>
          if (taxCodes.isEmpty)
            Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(declarationId, _).isDefined)
            if (allTaxCodesExistInACC14) {
              val newDeclarationReimbursementClaims = answers.correctedAmounts.flatMap(_.get(declarationId)) match {
                case None                      =>
                  Map(taxCodes.map(taxCode => taxCode -> None): _*)

                case Some(reimbursementClaims) =>
                  Map(taxCodes.map { taxCode =>
                    taxCode -> reimbursementClaims.get(taxCode).flatten
                  }: _*)
              }

              Right(
                new OverpaymentsMultipleJourney(
                  answers.copy(correctedAmounts =
                    answers.correctedAmounts
                      .orElse(Some(Map.empty[MRN, Map[TaxCode, Option[BigDecimal]]]))
                      .map(_.updated(declarationId, newDeclarationReimbursementClaims))
                      .map(OrderedMap(_))
                  )
                )
              )
            } else
              Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
          }
      }
    }

  def isValidCorrectAmount(correctAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    correctAmount >= 0 && correctAmount < BigDecimal(ndrcDetails.amount)

  def submitCorrectAmount(
    declarationId: MRN,
    taxCode: TaxCode,
    correctAmount: BigDecimal
  ): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      getDisplayDeclarationFor(declarationId) match {
        case None =>
          Left("submitCorrectAmount.missingDisplayDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(declarationId, taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(correctAmount, ndrcDetails) =>
              if (getSelectedDuties(declarationId).exists(_.contains(taxCode))) {
                val newCorrectedAmounts = answers.correctedAmounts.flatMap(_.get(declarationId)) match {
                  case None                   => Map(taxCode -> Some(correctAmount))
                  case Some(correctedAmounts) => correctedAmounts + (taxCode -> Some(correctAmount))
                }
                Right(
                  new OverpaymentsMultipleJourney(
                    answers.copy(correctedAmounts =
                      answers.correctedAmounts
                        .orElse(Some(Map.empty[MRN, Map[TaxCode, Option[BigDecimal]]]))
                        .map(_.updated(declarationId, newCorrectedAmounts))
                        .map(OrderedMap(_))
                    )
                  )
                )
              } else
                Left("submitCorrectAmount.taxCodeNotSelectedYet")

            case _ =>
              Left("submitCorrectAmount.invalidReimbursementAmount")
          }
      }
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new OverpaymentsMultipleJourney(
            answers.copy(bankAccountDetails = Some(bankAccountDetails))
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      if (needsBanksAccountDetailsSubmission)
        Right(
          new OverpaymentsMultipleJourney(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      new OverpaymentsMultipleJourney(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def resetBankTypeAndDetails(): OverpaymentsMultipleJourney =
    new OverpaymentsMultipleJourney(
      answers.copy(
        bankAccountType = None,
        bankAccountDetails = None
      )
    )

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def receiveUploadedFiles(
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      if (answers.nonce.equals(requestNonce)) {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(documentType))
          case uf                            => uf
        }
        Right(
          new OverpaymentsMultipleJourney(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsMultipleJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => new OverpaymentsMultipleJourney(answers.copy(checkYourAnswersChangeMode = enabled))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, OverpaymentsMultipleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ => Right(new OverpaymentsMultipleJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[OverpaymentsMultipleJourney]) {
      val that = obj.asInstanceOf[OverpaymentsMultipleJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"OverpaymentsMultipleJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[Seq[String], OverpaymentsMultipleJourney.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for {
          mrns                   <- answers.movementReferenceNumbers
          basisOfClaim           <- answers.basisOfClaim
          additionalDetails      <- answers.additionalDetails
          supportingEvidences     = answers.supportingEvidences
          claimantInformation    <- getClaimantInformation
          whetherNorthernIreland <- answers.whetherNorthernIreland
        } yield OverpaymentsMultipleJourney.Output(
          movementReferenceNumbers = mrns,
          claimantType = getClaimantType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          whetherNorthernIreland = whetherNorthernIreland,
          additionalDetails = additionalDetails,
          reimbursementClaims = OrderedMap(getReimbursementClaims),
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object OverpaymentsMultipleJourney extends JourneyCompanion[OverpaymentsMultipleJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): OverpaymentsMultipleJourney =
    new OverpaymentsMultipleJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type CorrectedAmounts = Map[TaxCode, Option[BigDecimal]]

  // All user answers captured during C&E1179 single MRN journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumbers: Option[Seq[MRN]] = None,
    displayDeclarations: Option[Seq[DisplayDeclaration]] = None,
    whetherNorthernIreland: Option[Boolean] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfOverpaymentClaim] = None,
    additionalDetails: Option[String] = None,
    correctedAmounts: Option[OrderedMap[MRN, CorrectedAmounts]] = None,
    bankAccountType: Option[BankAccountType] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    checkYourAnswersChangeMode: Boolean = false,
    dutiesChangeMode: Boolean = false
  ) extends OverpaymentsAnswers

  // Final minimal output of the journey we want to pass to the backend.
  final case class Output(
    movementReferenceNumbers: Seq[MRN],
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    whetherNorthernIreland: Boolean,
    additionalDetails: String,
    reimbursementClaims: OrderedMap[MRN, Map[TaxCode, BigDecimal]],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._
  import JourneyValidationErrors._

  object Checks extends OverpaymentsJourneyChecks[OverpaymentsMultipleJourney] {

    val hasMultipleMovementReferenceNumbers: Validate[OverpaymentsMultipleJourney] =
      checkIsTrue(_.answers.movementReferenceNumbers.exists(_.size > 1), MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER)

  }

  import Checks._

  /** Validate if all required answers has been provided and the journey is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsMultipleJourney] =
    all(
      hasMRNAndDisplayDeclaration,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided,
      hasMultipleMovementReferenceNumbers
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

  implicit val format: Format[OverpaymentsMultipleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new OverpaymentsMultipleJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  /** Try to build journey from the pre-existing answers. */
  override def tryBuildFrom(answers: Answers): Either[String, OverpaymentsMultipleJourney] =
    empty(answers.userEoriNumber, answers.nonce)
      .flatMapEachWhenDefined(answers.movementReferenceNumbers.zip(answers.displayDeclarations).zipWithIndex)(j => {
        case ((mrn: MRN, decl: DisplayDeclaration), index: Int) =>
          j.submitMovementReferenceNumberAndDeclaration(index, mrn, decl)
      })
      .flatMapWhenDefined(answers.consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(answers.declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress _)
      .mapWhenDefined(answers.whetherNorthernIreland)(_.submitWhetherNorthernIreland)
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapEachWhenDefined(answers.correctedAmounts)(j => {
        case (mrn: MRN, reimbursements: Map[TaxCode, Option[BigDecimal]]) =>
          j.selectAndReplaceTaxCodeSetForReimbursement(mrn, reimbursements.keySet.toSeq)
            .flatMapEachWhenMappingDefined(reimbursements)(j =>
              (taxCode, amount) => j.submitCorrectAmount(mrn, taxCode, amount)
            )
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
