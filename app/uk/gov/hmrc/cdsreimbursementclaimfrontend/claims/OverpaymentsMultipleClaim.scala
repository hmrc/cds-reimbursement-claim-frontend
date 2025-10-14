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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import cats.syntax.eq.*
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.*

import java.time.Instant
import java.time.LocalDateTime
import scala.collection.immutable.SortedMap

/** An encapsulated C285 multiple MRN claim logic. The constructor of this class MUST stay PRIVATE to protected
  * integrity of the claim.
  *
  * The claim uses two nested case classes:
  *
  *   - [[OverpaymentsMultipleClaim.Answers]] - keeps record of user answers and acquired documents
  *   - [[OverpaymentsMultipleClaim.Output]] - final output of the claim to be sent to backend processing
  */
final class OverpaymentsMultipleClaim private (
  val answers: OverpaymentsMultipleClaim.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[OverpaymentsMultipleClaim.Features]
) extends ClaimBase
    with DirectFluentSyntax[OverpaymentsMultipleClaim]
    with OverpaymentsClaimProperties
    with ClaimAnalytics {

  type Type = OverpaymentsMultipleClaim

  val self: OverpaymentsMultipleClaim = this

  val validate: Validator.Validate[OverpaymentsMultipleClaim] =
    OverpaymentsMultipleClaim.validator

  private def copy(
    newAnswers: OverpaymentsMultipleClaim.Answers
  ): OverpaymentsMultipleClaim =
    new OverpaymentsMultipleClaim(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.correctedAmounts.exists(claims => claims.values.forall(rc => rc.nonEmpty && rc.forall(_._2.isDefined)))

  def getMovementReferenceNumbers: Option[Seq[MRN]] =
    answers.movementReferenceNumbers

  def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumbers.flatMap(_.headOption)

  def getLeadImportDeclaration: Option[ImportDeclaration] =
    answers.importDeclarations.flatMap(_.headOption)

  def getImportDeclarationFor(mrn: MRN): Option[ImportDeclaration] =
    for
      declarations <- answers.importDeclarations
      declaration  <- declarations.find(_.getMRN === mrn)
    yield declaration

  def getNthImportDeclaration(index: Int): Option[ImportDeclaration] =
    getNthMovementReferenceNumber(index).flatMap(getImportDeclarationFor)

  override def getImportDeclarations: Seq[ImportDeclaration] =
    answers.importDeclarations.getOrElse(Seq.empty)

  def getNthMovementReferenceNumber(index: Int): Option[MRN] =
    answers.movementReferenceNumbers.flatMap { mrns =>
      if index >= 0 && index < mrns.size then Some(mrns(index))
      else None
    }

  def getIndexOfMovementReferenceNumber(mrn: MRN): Option[Int] =
    answers.movementReferenceNumbers.flatMap(_.zipWithIndex.find(_._1 === mrn).map(_._2))

  def countOfMovementReferenceNumbers: Int =
    answers.movementReferenceNumbers.map(_.size).getOrElse(0)

  def hasCompleteMovementReferenceNumbers: Boolean =
    countOfMovementReferenceNumbers >= 2

  def needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex: Int): Boolean =
    if pageIndex === 1 then needsDeclarantAndConsigneeEoriSubmission else false

  def needsDuplicateMrnAndDeclaration: Boolean =
    answers.basisOfClaim.contains(BasisOfOverpaymentClaim.DuplicateEntry)

  def getNdrcDetails: Option[List[NdrcDetails]] =
    getLeadImportDeclaration.flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(mrn: MRN): Option[List[NdrcDetails]] =
    getImportDeclarationFor(mrn).flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(declarationid: MRN, taxCode: TaxCode): Option[NdrcDetails] =
    getImportDeclarationFor(declarationid).flatMap(_.getNdrcDetailsFor(taxCode.value))

  /** Returns the amount paid for the given MRN and tax code as returned by ACC14, or None if either MRN or tax code not
    * found.
    */
  def getAmountPaidFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    getNdrcDetailsFor(mrn, taxCode).map(_.amount).map(BigDecimal.apply)

  /** If the user has selected the tax code for repayment then returns the amount paid for the given MRN and tax code as
    * returned by ACC14, otherwise None.
    */
  def getAmountPaidForIfSelected(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    getSelectedDuties(mrn)
      .flatMap(selectedTaxCodes =>
        if selectedTaxCodes.contains(taxCode) then getAmountPaidFor(mrn, taxCode)
        else None
      )

  def getCorrectedAmountFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    for
      all           <- answers.correctedAmounts
      thisMrn       <- all.get(mrn)
      correctAmount <- thisMrn.get(taxCode).flatten
    yield correctAmount

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
        if taxCodes.isEmpty then None else Some(taxCodes)
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
      .exists(isAllSelectedDutiesAreCMAEligible)

  def getAvailableTaxCodesWithPaidAmountsFor(declarationId: MRN): Seq[(TaxCode, BigDecimal)] =
    getImportDeclarationFor(declarationId)
      .flatMap(_.getNdrcDutiesWithAmount)
      .getOrElse(Seq.empty)

  def getReimbursementClaimsFor(declarationId: MRN): OrderedMap[TaxCode, BigDecimal] = {
    val taxCodesWithPaidAmounts: Map[TaxCode, BigDecimal] =
      getAvailableTaxCodesWithPaidAmountsFor(declarationId).toMap

    answers.correctedAmounts
      .flatMap(
        _.get(declarationId)
      )
      .map(x =>
        OrderedMap.from(
          x.map { case (taxCode, correctAmountOpt) =>
            for
              correctAmount <- correctAmountOpt
              paidAmount    <- taxCodesWithPaidAmounts.get(taxCode)
            yield (taxCode, paidAmount - correctAmount)
          }.collect { case Some(y) => y }
        )
      )
      .getOrElse(OrderedMap.empty)
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
              for
                correctAmount <- correctAmountOpt
                paidAmount    <- taxCodesWithPaidAmounts.get(taxCode)
              yield (taxCode, paidAmount - correctAmount)
            }
            .collect { case Some(x) => x }
            .toMap
        )
      })
      .getOrElse(Map.empty)

  def getReimbursementsWithCorrectAmounts: Seq[(MRN, Int, List[ReimbursementWithCorrectAmount])] =
    getReimbursementClaims.toSeq.zipWithIndex
      .map { case ((mrn, _), index) => (mrn, index + 1, getReimbursementWithCorrectAmountFor(mrn)) }

  def getReimbursementWithCorrectAmountFor(declarationId: MRN): List[ReimbursementWithCorrectAmount] = {
    val taxCodesWithAmountPaidAndCorrect = getAvailableTaxCodesWithPaidAmountsFor(declarationId)
      .flatMap { case (taxCode, paidAmount) =>
        getCorrectedAmountFor(declarationId, taxCode) match {
          case Some(ca) => Some((taxCode, Some(AmountPaidWithCorrect(paidAmount, ca))))
          case _        => None
        }
      }
    toReimbursementWithCorrectAmount(SortedMap(taxCodesWithAmountPaidAndCorrect*))
  }

  def hasAllClaimsSelectedForIndex(index: Int): Boolean =
    getNthMovementReferenceNumber(index - 1).forall { mrn =>
      getSelectedDuties(mrn).map(_.size).getOrElse(0) == getAvailableDuties(mrn).size
    }

  def getReimbursementAmountForDeclaration(declarationId: MRN): BigDecimal =
    getReimbursementClaimsFor(declarationId).toSeq.map(_._2).sum

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.flatMap(_._2.values).sum

  def withDutiesChangeMode(enabled: Boolean): OverpaymentsMultipleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): OverpaymentsMultipleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.overpaymentsSingleDocumentTypes)

  def getAvailableClaimTypes: Set[BasisOfOverpaymentClaim] =
    BasisOfOverpaymentClaim
      .excludeNorthernIrelandClaims(
        hasDuplicateEntryClaim = false,
        getLeadImportDeclaration,
        isOtherEnabled = features.exists(_.shouldAllowOtherBasisOfClaim)
      )

  def containsUnsupportedTaxCodeFor(mrn: MRN): Boolean =
    getImportDeclarationFor(mrn).exists(_.containsSomeUnsupportedTaxCode)

  def removeUnsupportedTaxCodes(): OverpaymentsMultipleClaim =
    this.copy(answers.copy(importDeclarations = answers.importDeclarations.map(_.map(_.removeUnsupportedTaxCodes()))))

  /** Resets the claim with the new MRN or keep existing claim if submitted the same MRN and declaration as before.
    */
  def submitMovementReferenceNumberAndDeclaration(
    index: Int,
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      if index < 0 then Left("submitMovementReferenceNumber.negativeIndex")
      else if index > countOfMovementReferenceNumbers then Left("submitMovementReferenceNumber.invalidIndex")
      else if mrn =!= importDeclaration.getMRN then
        Left(
          "submitMovementReferenceNumber.wrongImportDeclarationMrn"
        )
      else if index > 0 &&
        !getLeadImportDeclaration.exists(importDeclaration.hasSameEoriAs)
      then
        Left(
          "submitMovementReferenceNumber.wrongImportDeclarationEori"
        )
      else
        getNthMovementReferenceNumber(index) match {
          // do nothing if MRN value and positions does not change, and declaration is the same
          case Some(existingMrn)
              if existingMrn === mrn &&
                getImportDeclarationFor(mrn).contains(importDeclaration) =>
            Right(this)

          // change an existing MRN
          case Some(existingMrn) =>
            if getIndexOfMovementReferenceNumber(mrn).exists(_ =!= index) then
              Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
            else if index === 0 then {
              // first MRN change resets all the claim
              Right(
                new OverpaymentsMultipleClaim(
                  OverpaymentsMultipleClaim
                    .Answers(
                      userEoriNumber = answers.userEoriNumber,
                      movementReferenceNumbers = Some(Seq(mrn)),
                      importDeclarations = Some(Seq(importDeclaration)),
                      eoriNumbersVerification = answers.eoriNumbersVerification.map(_.keepUserXiEoriOnly),
                      nonce = answers.nonce
                    ),
                  startTimeSeconds = this.startTimeSeconds,
                  features = features
                )
              )
            } else {
              // change of an existing MRN removes related declaration and claims
              Right(
                this.copy(
                  answers.copy(
                    movementReferenceNumbers = answers.movementReferenceNumbers
                      .map(mrns => (mrns.take(index) :+ mrn) ++ mrns.drop(index + 1)),
                    importDeclarations = answers.importDeclarations.map(
                      _.filterNot(_.displayResponseDetail.declarationId === existingMrn.value) :+ importDeclaration
                    ),
                    correctedAmounts =
                      answers.correctedAmounts.map(_.removed(existingMrn).updated(mrn, OrderedMap.empty))
                  )
                )
              )
            }

          // add new MRN
          case None              =>
            if getIndexOfMovementReferenceNumber(mrn).isDefined then
              Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
            else
              Right(
                this.copy(
                  answers.copy(
                    movementReferenceNumbers = answers.movementReferenceNumbers.map(_ :+ mrn).orElse(Some(Seq(mrn))),
                    importDeclarations =
                      answers.importDeclarations.map(_ :+ importDeclaration).orElse(Some(Seq(importDeclaration))),
                    correctedAmounts = answers.correctedAmounts
                      .map(_ + (mrn -> OrderedMap.empty[TaxCode, Option[BigDecimal]]))
                      .orElse(Some(OrderedMap(mrn -> OrderedMap.empty[TaxCode, Option[BigDecimal]])))
                  )
                )
              )
        }
    }

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ): Either[String, OverpaymentsMultipleClaim] =
    submitMovementReferenceNumberAndDeclaration(0, mrn, importDeclaration)

  def removeMovementReferenceNumberAndImportDeclaration(mrn: MRN): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      getIndexOfMovementReferenceNumber(mrn) match {
        case None                                             => Left("removeMovementReferenceNumberAndImportDeclaration.notFound")
        case Some(0)                                          => Left("removeMovementReferenceNumberAndImportDeclaration.cannotRemoveFirstMRN")
        case Some(1) if countOfMovementReferenceNumbers === 2 =>
          Left("removeMovementReferenceNumberAndImportDeclaration.cannotRemoveSecondMRN")
        case Some(index)                                      =>
          Right(
            this.copy(
              answers.copy(
                movementReferenceNumbers = answers.movementReferenceNumbers
                  .map(mrns => mrns.take(index) ++ mrns.drop(index + 1)),
                importDeclarations = answers.importDeclarations.map(
                  _.filterNot(_.displayResponseDetail.declarationId === mrn.value)
                ),
                correctedAmounts = answers.correctedAmounts.map(_.removed(mrn))
              )
            )
          )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      if needsDeclarantAndConsigneeEoriSubmission then
        if getConsigneeEoriFromACC14 match {
            case Some(eori) => eori === consigneeEoriNumber
            case None       => getDeclarantEoriFromACC14.contains(consigneeEoriNumber)
          }
        then
          Right(
            this.copy(
              answers.copy(eoriNumbersVerification =
                answers.eoriNumbersVerification
                  .orElse(Some(EoriNumbersVerification()))
                  .map(_.copy(consigneeEoriNumber = Some(consigneeEoriNumber)))
              )
            )
          )
        else Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      if needsDeclarantAndConsigneeEoriSubmission then
        if getDeclarantEoriFromACC14.contains(declarantEoriNumber) then
          Right(
            this.copy(
              answers.copy(eoriNumbersVerification =
                answers.eoriNumbersVerification
                  .orElse(Some(EoriNumbersVerification()))
                  .map(_.copy(declarantEoriNumber = Some(declarantEoriNumber)))
              )
            )
          )
        else Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfOverpaymentClaim): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      basisOfClaim match {
        case BasisOfOverpaymentClaim.DuplicateEntry =>
          this.copy(answers.copy(basisOfClaim = Some(basisOfClaim)))

        case _ =>
          this.copy(
            answers.copy(
              basisOfClaim = Some(basisOfClaim)
            )
          )
      }
    }

  def submitNewEori(eori: Eori): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newEori = Some(eori))
      )
    }

  def submitNewDan(dan: Dan): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(newDan = Some(dan))
      )
    }

  def submitAdditionalDetails(
    additionalDetails: String
  ): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(additionalDetails = Some(additionalDetails))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    declarationId: MRN,
    taxCodes: Seq[TaxCode]
  ): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      getImportDeclarationFor(declarationId) match {
        case None => Left("selectTaxCodeSetForReimbursement.missingImportDeclaration")

        case Some(_) =>
          if taxCodes.isEmpty then Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(declarationId, _).isDefined)
            if allTaxCodesExistInACC14 then {
              val newDeclarationReimbursementClaims = answers.correctedAmounts.flatMap(_.get(declarationId)) match {
                case None =>
                  OrderedMap[TaxCode, Option[BigDecimal]](taxCodes.map(taxCode => taxCode -> None)*)

                case Some(reimbursementClaims) =>
                  OrderedMap(taxCodes.map { taxCode =>
                    taxCode -> reimbursementClaims.get(taxCode).flatten
                  }*)
              }

              Right(
                this.copy(
                  answers.copy(correctedAmounts =
                    answers.correctedAmounts
                      .orElse(Some(OrderedMap.empty[MRN, OrderedMap[TaxCode, Option[BigDecimal]]]))
                      .map(_.updated(declarationId, newDeclarationReimbursementClaims))
                  )
                )
              )
            } else Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
          }
      }
    }

  def isValidCorrectAmount(correctAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    correctAmount >= 0 && correctAmount < BigDecimal(ndrcDetails.amount)

  def submitCorrectAmount(
    declarationId: MRN,
    taxCode: TaxCode,
    correctAmount: BigDecimal
  ): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      getImportDeclarationFor(declarationId) match {
        case None =>
          Left("submitCorrectAmount.missingImportDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(declarationId, taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(correctAmount, ndrcDetails) =>
              if getSelectedDuties(declarationId).exists(_.contains(taxCode)) then {
                val newCorrectedAmounts = answers.correctedAmounts.flatMap(_.get(declarationId)) match {
                  case None                   => OrderedMap[TaxCode, Option[BigDecimal]](taxCode -> Some(correctAmount))
                  case Some(correctedAmounts) => correctedAmounts + (taxCode -> Some(correctAmount))
                }
                Right(
                  this.copy(
                    answers.copy(correctedAmounts =
                      answers.correctedAmounts
                        .orElse(Some(OrderedMap.empty[MRN, OrderedMap[TaxCode, Option[BigDecimal]]]))
                        .map(_.updated(declarationId, newCorrectedAmounts))
                    )
                  )
                )
              } else Left("submitCorrectAmount.taxCodeNotSelectedYet")

            case _ =>
              Left("submitCorrectAmount.invalidReimbursementAmount")
          }
      }
    }

  def submitClaimAmount(
    declarationId: MRN,
    taxCode: TaxCode,
    claimAmount: BigDecimal
  ): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      getImportDeclarationFor(declarationId) match {
        case None =>
          Left("submitCorrectAmount.missingImportDeclaration")

        case Some(_) =>
          getNdrcDetailsFor(declarationId, taxCode) match {
            case None =>
              Left("submitCorrectAmount.taxCodeNotInACC14")

            case Some(ndrcDetails) if isValidCorrectAmount(BigDecimal(ndrcDetails.amount) - claimAmount, ndrcDetails) =>
              if getSelectedDuties(declarationId).exists(_.contains(taxCode)) then {
                val correctAmount       = BigDecimal(ndrcDetails.amount) - claimAmount
                val newCorrectedAmounts = answers.correctedAmounts.flatMap(_.get(declarationId)) match {
                  case None                   => OrderedMap[TaxCode, Option[BigDecimal]](taxCode -> Some(correctAmount))
                  case Some(correctedAmounts) => correctedAmounts + (taxCode -> Some(correctAmount))
                }
                Right(
                  this.copy(
                    answers.copy(correctedAmounts =
                      answers.correctedAmounts
                        .orElse(Some(OrderedMap.empty[MRN, OrderedMap[TaxCode, Option[BigDecimal]]]))
                        .map(_.updated(declarationId, newCorrectedAmounts))
                    )
                  )
                )
              } else Left("submitCorrectAmount.taxCodeNotSelectedYet")

            case _ =>
              Left("submitCorrectAmount.invalidReimbursementAmount")
          }
      }
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      if answers.payeeType.contains(payeeType) then Right(this)
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountDetails =
            Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
          )
        )
      )
    }

  def removeBankAccountDetails(): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def resetBankTypeAndDetails(): OverpaymentsMultipleClaim =
    this.copy(
      answers.copy(
        bankAccountType = None,
        bankAccountDetails = None
      )
    )

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      if answers.nonce.equals(requestNonce) then {
        val uploadedFilesWithDocumentTypeAdded = uploadedFiles.map {
          case uf if uf.documentType.isEmpty => uf.copy(cargo = documentType)
          case uf                            => uf
        }
        Right(
          this.copy(answers.copy(supportingEvidences = uploadedFilesWithDocumentTypeAdded))
        )
      } else Left("receiveUploadedFiles.invalidNonce")
    }

  def submitCheckYourAnswersChangeMode(enabled: Boolean): OverpaymentsMultipleClaim =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeClaimWith(caseNumber: String): Either[String, OverpaymentsMultipleClaim] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new OverpaymentsMultipleClaim(
                answers = this.answers,
                startTimeSeconds = this.startTimeSeconds,
                caseNumber = Some(caseNumber),
                submissionDateTime = Some(LocalDateTime.now()),
                features = features
              )
            )
        )
    }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[OverpaymentsMultipleClaim] then {
      val that = obj.asInstanceOf[OverpaymentsMultipleClaim]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"OverpaymentsMultipleClaim${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the claim and retrieves the output. */

  def toOutput: Either[Seq[String], OverpaymentsMultipleClaim.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for
          mrns                <- answers.movementReferenceNumbers
          basisOfClaim        <- answers.basisOfClaim
          additionalDetails   <- answers.additionalDetails
          supportingEvidences  = answers.supportingEvidences
          claimantInformation <- getClaimantInformation
          payeeType           <- getPayeeTypeForOutput(answers.payeeType)
          displayPayeeType    <- answers.payeeType
          newEoriAndDan        = (basisOfClaim, answers.newEori, answers.newDan) match {
                                   case (IncorrectEoriAndDan, Some(newEori), Some(newDan)) =>
                                     Some(NewEoriAndDan(newEori, newDan.value))
                                   case _                                                  => None
                                 }
        yield OverpaymentsMultipleClaim.Output(
          movementReferenceNumbers = mrns,
          claimantType = getClaimantType,
          payeeType = payeeType,
          displayPayeeType = displayPayeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          additionalDetails = additionalDetails,
          reimbursementClaims = OrderedMap(getReimbursementClaims),
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails,
          newEoriAndDan = newEoriAndDan
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object OverpaymentsMultipleClaim extends ClaimCompanion[OverpaymentsMultipleClaim] {

  /** A starting point to build new instance of the claim. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): OverpaymentsMultipleClaim =
    new OverpaymentsMultipleClaim(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = OrderedMap[TaxCode, Option[BigDecimal]]

  final case class Features(
    shouldAllowOtherBasisOfClaim: Boolean = true
  )

  // All user answers captured during C&E1179 single MRN claim
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumbers: Option[Seq[MRN]] = None,
    payeeType: Option[PayeeType] = None,
    importDeclarations: Option[Seq[ImportDeclaration]] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfOverpaymentClaim] = None,
    additionalDetails: Option[String] = None,
    correctedAmounts: Option[OrderedMap[MRN, CorrectedAmounts]] = None,
    bankAccountType: Option[BankAccountType] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    newEori: Option[Eori] = None,
    newDan: Option[Dan] = None,
    modes: ClaimModes = ClaimModes()
  ) extends OverpaymentsAnswers

  // Final minimal output of the claim we want to pass to the backend.
  final case class Output(
    movementReferenceNumbers: Seq[MRN],
    claimantType: ClaimantType,
    payeeType: PayeeType,
    displayPayeeType: PayeeType,
    claimantInformation: ClaimantInformation,
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    reimbursementClaims: OrderedMap[MRN, Map[TaxCode, BigDecimal]],
    reimbursementMethod: ReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument],
    newEoriAndDan: Option[NewEoriAndDan]
  ) extends WafErrorMitigation[Output] {

    override def excludeFreeTextInputs() =
      (
        Seq(("additional_details", additionalDetails)),
        this.copy(additionalDetails = additionalDetailsReplacementText)
      )
  }

  import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator._
  import ClaimValidationErrors._

  object Checks extends OverpaymentsClaimChecks[OverpaymentsMultipleClaim] {

    val hasMultipleMovementReferenceNumbers: Validate[OverpaymentsMultipleClaim] =
      checkIsTrue(_.answers.movementReferenceNumbers.exists(_.size > 1), MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER)

  }

  import Checks._

  /** Validate if all required answers has been provided and the claim is ready to produce output. */
  override implicit val validator: Validate[OverpaymentsMultipleClaim] =
    all(
      hasMRNAndImportDeclaration,
      containsOnlySupportedTaxCodes,
      declarantOrImporterEoriMatchesUserOrHasBeenVerified,
      basisOfClaimHasBeenProvided,
      additionalDetailsHasBeenProvided,
      reimbursementClaimsHasBeenProvided,
      paymentMethodHasBeenProvidedIfNeeded,
      contactDetailsHasBeenProvided,
      supportingEvidenceHasBeenProvided,
      hasMultipleMovementReferenceNumbers,
      declarationsHasNoSubsidyPayments,
      payeeTypeIsDefined,
      newEoriAndDanProvidedIfNeeded
    )

  import ClaimFormats._

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

  implicit val format: Format[OverpaymentsMultipleClaim] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new OverpaymentsMultipleClaim(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(claim =>
        (claim.answers, claim.startTimeSeconds, claim.caseNumber, claim.submissionDateTime, claim.features)
      )
    )

  /** Try to build claim from the pre-existing answers. */
  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, OverpaymentsMultipleClaim] =
    empty(answers.userEoriNumber, answers.nonce, features)
      .flatMapEachWhenDefined(answers.movementReferenceNumbers.zipOpt(answers.importDeclarations).zipWithIndex)(j => {
        case ((mrn: MRN, decl: ImportDeclaration), index: Int) =>
          j.submitMovementReferenceNumberAndDeclaration(index, mrn, decl)
      })
      .mapWhenDefined(answers.eoriNumbersVerification.flatMap(_.userXiEori))(_.submitUserXiEori)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber))(_.submitConsigneeEoriNumber)
      .flatMapWhenDefined(answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber))(_.submitDeclarantEoriNumber)
      .map(_.submitContactDetails(answers.contactDetails))
      .mapWhenDefined(answers.contactAddress)(_.submitContactAddress)
      .map(_.withEnterContactDetailsMode(answers.modes.enterContactDetailsMode))
      .mapWhenDefined(answers.basisOfClaim)(_.submitBasisOfClaim)
      .mapWhenDefined(answers.additionalDetails)(_.submitAdditionalDetails)
      .flatMapEachWhenDefined(answers.correctedAmounts)(j => {
        case (mrn: MRN, reimbursements: Map[TaxCode, Option[BigDecimal]]) =>
          j.selectAndReplaceTaxCodeSetForReimbursement(mrn, reimbursements.keySet.toSeq)
            .flatMapEachWhenMappingDefined(reimbursements)(j =>
              (taxCode, amount) => j.submitCorrectAmount(mrn, taxCode, amount)
            )
      })
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType)
      .mapWhenDefined(answers.newEori)(_.submitNewEori)
      .mapWhenDefined(answers.newDan)(_.submitNewDan)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

  /** This method MUST BE used only to test the validation correctness of the invalid answer states. */
  def unsafeModifyAnswers(
    claim: OverpaymentsMultipleClaim,
    f: OverpaymentsMultipleClaim.Answers => OverpaymentsMultipleClaim.Answers
  ): OverpaymentsMultipleClaim =
    OverpaymentsMultipleClaim(
      answers = f(claim.answers),
      startTimeSeconds = claim.startTimeSeconds,
      features = claim.features
    )
}
