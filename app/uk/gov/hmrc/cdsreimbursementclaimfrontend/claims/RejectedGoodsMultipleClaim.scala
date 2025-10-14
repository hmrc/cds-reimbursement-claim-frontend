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

import cats.Eq
import cats.syntax.eq.*
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.*

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import scala.collection.immutable.SortedMap

/** An encapsulated C&E1179 multiple MRN claim logic. The constructor of this class MUST stay PRIVATE to protected
  * integrity of the claim.
  *
  * The claim uses two nested case classes:
  *
  *   - [[RejectedGoodsMultipleClaim.Answers]] - keeps record of user answers and acquired documents
  *   - [[RejectedGoodsMultipleClaim.Output]] - final outcome of the claim to be sent to backend processing
  */
final class RejectedGoodsMultipleClaim private (
  val answers: RejectedGoodsMultipleClaim.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[RejectedGoodsMultipleClaim.Features]
) extends ClaimBase
    with DirectFluentSyntax[RejectedGoodsMultipleClaim]
    with RejectedGoodsClaimProperties
    with HaveInspectionDetails
    with ClaimAnalytics {

  type Type = RejectedGoodsMultipleClaim

  val self: RejectedGoodsMultipleClaim = this

  val validate: Validator.Validate[RejectedGoodsMultipleClaim] =
    RejectedGoodsMultipleClaim.validator

  private def copy(
    newAnswers: RejectedGoodsMultipleClaim.Answers
  ): RejectedGoodsMultipleClaim =
    new RejectedGoodsMultipleClaim(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

  /** Check if all the selected duties have reimbursement amount provided. */
  def hasCompleteReimbursementClaims: Boolean =
    answers.correctedAmounts.exists(mrc =>
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
      if index >= 0 && index < mrns.size then Some(mrns(index))
      else None
    }

  def getIndexOfMovementReferenceNumber(mrn: MRN): Option[Int] =
    answers.movementReferenceNumbers.flatMap(_.zipWithIndex.find(_._1 === mrn).map(_._2))

  def countOfMovementReferenceNumbers: Int =
    answers.movementReferenceNumbers.map(_.size).getOrElse(0)

  def hasCompleteMovementReferenceNumbers: Boolean =
    countOfMovementReferenceNumbers >= 2

  def getLeadImportDeclaration: Option[ImportDeclaration] =
    getLeadMovementReferenceNumber.flatMap(getImportDeclarationFor)

  def getNthImportDeclaration(index: Int): Option[ImportDeclaration] =
    getNthMovementReferenceNumber(index).flatMap(getImportDeclarationFor)

  def getImportDeclarationFor(mrn: MRN): Option[ImportDeclaration] =
    for
      declarations <- answers.importDeclarations
      declaration  <- declarations.find(_.getMRN === mrn)
    yield declaration

  override def getImportDeclarations: Seq[ImportDeclaration] =
    answers.importDeclarations.getOrElse(Seq.empty)

  def getCorrectAmountsFor(mrn: MRN): Option[OrderedMap[TaxCode, Option[BigDecimal]]] =
    answers.correctedAmounts.flatMap(_.get(mrn))

  def getCorrectedAmountFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    for
      all           <- answers.correctedAmounts
      thisMrn       <- all.get(mrn)
      correctAmount <- thisMrn.get(taxCode).flatten
    yield correctAmount

  def getAvailableTaxCodesWithPaidAmountsFor(declarationId: MRN): Seq[(TaxCode, BigDecimal)] =
    getImportDeclarationFor(declarationId)
      .flatMap(_.getNdrcDutiesWithAmount)
      .getOrElse(Seq.empty)

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

  def hasAllClaimsSelectedForIndex(index: Int): Boolean = true

  def needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex: Int): Boolean =
    if pageIndex === 1 then needsDeclarantAndConsigneeEoriSubmission else false

  def getNdrcDetailsFor(mrn: MRN): Option[List[NdrcDetails]] =
    getImportDeclarationFor(mrn).flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(mrn: MRN, taxCode: TaxCode): Option[NdrcDetails] =
    getImportDeclarationFor(mrn).flatMap(_.getNdrcDetailsFor(taxCode.value))

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

  def getSelectedDuties(mrn: MRN): Option[Seq[TaxCode]] =
    getCorrectAmountsFor(mrn).map(_.keys.toSeq)

  def getAllSelectedDuties: Seq[(MRN, Seq[TaxCode])] =
    answers.movementReferenceNumbers
      .map(_.map { mrn =>
        (mrn, getSelectedDuties(mrn).getOrElse(Seq.empty))
      })
      .getOrElse(Seq.empty)

  def isAllSelectedDutiesAreCMAEligible: Boolean =
    answers.correctedAmounts
      .map(_.flatMap { case (mrn, rc) =>
        rc.keySet
          .map(getNdrcDetailsFor(mrn, _))
          .collect { case Some(d) => d }
      })
      .exists(_.forall(_.isCmaEligible))

  def getTotalReimbursementAmountFor(mrn: MRN): Option[BigDecimal] =
    getReimbursementClaims.get(mrn).map(_.map(_._2).sum)

  def getTotalReimbursementAmount: BigDecimal =
    getReimbursementClaims.values.flatMap(_.values).sum

  def withDutiesChangeMode(enabled: Boolean): RejectedGoodsMultipleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): RejectedGoodsMultipleClaim =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsMultipleDocumentTypes)

  def containsUnsupportedTaxCodeFor(mrn: MRN): Boolean =
    getImportDeclarationFor(mrn).exists(_.containsSomeUnsupportedTaxCode)

  def removeUnsupportedTaxCodes(): RejectedGoodsMultipleClaim =
    this.copy(answers.copy(importDeclarations = answers.importDeclarations.map(_.map(_.removeUnsupportedTaxCodes()))))

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ): Either[String, RejectedGoodsMultipleClaim] =
    submitMovementReferenceNumberAndDeclaration(0, mrn, importDeclaration)

  def submitMovementReferenceNumberAndDeclaration(
    index: Int,
    mrn: MRN,
    importDeclaration: ImportDeclaration
  ) =
    whileClaimIsAmendable {
      if index < 0 then Left("submitMovementReferenceNumber.negativeIndex")
      else if index > countOfMovementReferenceNumbers then Left("submitMovementReferenceNumber.invalidIndex")
      else if mrn =!= importDeclaration.getMRN then
        Left(
          "submitMovementReferenceNumber.wrongImportDeclarationMrn"
        )
      else if index > 0 && !getLeadImportDeclaration.exists(importDeclaration.hasSameEoriAs) then
        Left("submitMovementReferenceNumber.wrongImportDeclarationEori")
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
                new RejectedGoodsMultipleClaim(
                  RejectedGoodsMultipleClaim
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

  def removeMovementReferenceNumberAndImportDeclaration(mrn: MRN): Either[String, RejectedGoodsMultipleClaim] =
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

  def submitUserXiEori(userXiEori: UserXiEori): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsMultipleClaim] =
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
        else
          Left(
            ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI
          )
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsMultipleClaim] =
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

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsMultipleClaim =
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
  ): Either[String, RejectedGoodsMultipleClaim] =
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

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
      )
    }

  def selectAndReplaceTaxCodeSetForReimbursement(
    mrn: MRN,
    taxCodes: Seq[TaxCode]
  ): Either[String, RejectedGoodsMultipleClaim] =
    whileClaimIsAmendable {
      getImportDeclarationFor(mrn) match {
        case None =>
          Left("selectAndReplaceTaxCodeSetForReimbursement.missingImportDeclaration")

        case Some(_) =>
          if taxCodes.isEmpty then Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(mrn, _).isDefined)
            if allTaxCodesExistInACC14 then {
              val newCorrectAmounts: RejectedGoodsMultipleClaim.CorrectedAmounts =
                getCorrectAmountsFor(mrn) match {
                  case None =>
                    OrderedMap.from(taxCodes.map(taxCode => taxCode -> None))

                  case Some(correctAmounts) =>
                    OrderedMap.from(taxCodes.map { taxCode =>
                      taxCode -> correctAmounts.get(taxCode).flatten
                    })
                }

              Right(
                this.copy(
                  answers.copy(correctedAmounts =
                    answers.correctedAmounts
                      .map(_ + (mrn -> newCorrectAmounts))
                      .orElse(Some(OrderedMap(mrn -> newCorrectAmounts)))
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
  ): Either[String, RejectedGoodsMultipleClaim] =
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
  ): Either[String, RejectedGoodsMultipleClaim] =
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

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, RejectedGoodsMultipleClaim] =
    whileClaimIsAmendable {
      if answers.payeeType.contains(payeeType) then Right(copy(newAnswers = answers.copy(payeeType = Some(payeeType))))
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsMultipleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountDetails =
            Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
          )
        )
      )
    }

  def removeBankAccountDetails(): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsMultipleClaim] =
    whileClaimIsAmendable {
      Right(
        this.copy(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
      )
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsMultipleClaim] =
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

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsMultipleClaim =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeClaimWith(caseNumber: String): Either[String, RejectedGoodsMultipleClaim] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new RejectedGoodsMultipleClaim(
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
    if obj.isInstanceOf[RejectedGoodsMultipleClaim] then {
      val that = obj.asInstanceOf[RejectedGoodsMultipleClaim]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"RejectedGoodsMultipleClaim${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the claim and retrieves the output. */

  def toOutput: Either[Seq[String], RejectedGoodsMultipleClaim.Output] =
    validate(this).left
      .map(_.messages)
      .flatMap(_ =>
        (for
          movementReferenceNumbers <- answers.movementReferenceNumbers
          basisOfClaim             <- answers.basisOfClaim
          methodOfDisposal         <- answers.methodOfDisposal
          detailsOfRejectedGoods   <- answers.detailsOfRejectedGoods
          inspectionDate           <- answers.inspectionDate
          inspectionAddress        <- answers.inspectionAddress
          payeeType                <- getPayeeTypeForOutput(answers.payeeType)
          displayPayeeType         <- answers.payeeType
          supportingEvidences       = answers.supportingEvidences
          claimantInformation      <- getClaimantInformation
        yield RejectedGoodsMultipleClaim.Output(
          movementReferenceNumbers = movementReferenceNumbers,
          claimantType = getClaimantType,
          payeeType = payeeType,
          displayPayeeType = displayPayeeType,
          claimantInformation = claimantInformation,
          basisOfClaim = basisOfClaim,
          methodOfDisposal = methodOfDisposal,
          detailsOfRejectedGoods = detailsOfRejectedGoods,
          inspectionDate = inspectionDate,
          inspectionAddress = inspectionAddress,
          reimbursementClaims = OrderedMap(getReimbursementClaims),
          supportingEvidences = supportingEvidences.map(EvidenceDocument.from),
          basisOfClaimSpecialCircumstances = answers.basisOfClaimSpecialCircumstances,
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsMultipleClaim extends ClaimCompanion[RejectedGoodsMultipleClaim] {

  /** A starting point to build new instance of the claim. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): RejectedGoodsMultipleClaim =
    new RejectedGoodsMultipleClaim(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = OrderedMap[TaxCode, Option[BigDecimal]]

  final case class Features()

  // All user answers captured during C&E1179 multiple MRNs claim
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumbers: Option[Seq[MRN]] = None,
    importDeclarations: Option[Seq[ImportDeclaration]] = None,
    payeeType: Option[PayeeType] = None,
    eoriNumbersVerification: Option[EoriNumbersVerification] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    correctedAmounts: Option[OrderedMap[MRN, CorrectedAmounts]] = None,
    inspectionDate: Option[InspectionDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    selectedDocumentType: Option[UploadDocumentType] = None,
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    modes: ClaimModes = ClaimModes()
  ) extends RejectedGoodsAnswers

  // Final minimal output of the claim we want to pass to the backend.
  final case class Output(
    movementReferenceNumbers: Seq[MRN],
    claimantType: ClaimantType,
    payeeType: PayeeType,
    displayPayeeType: PayeeType,
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
  ) extends WafErrorMitigation[Output] {

    override def excludeFreeTextInputs() =
      (
        Seq(("additional_details", detailsOfRejectedGoods))
          ++ basisOfClaimSpecialCircumstances.map(v => Seq(("special_circumstances", v))).getOrElse(Seq.empty),
        this.copy(
          detailsOfRejectedGoods = additionalDetailsReplacementText,
          basisOfClaimSpecialCircumstances =
            basisOfClaimSpecialCircumstances.map(_ => specialCircumstancesReplacementText)
        )
      )
  }

  import ClaimValidationErrors._
  import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator._

  object Checks extends RejectedGoodsClaimChecks[RejectedGoodsMultipleClaim] {

    val hasMultipleMovementReferenceNumbers: Validate[RejectedGoodsMultipleClaim] =
      checkIsTrue(_.answers.movementReferenceNumbers.exists(_.size > 1), MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER)
  }

  import Checks._

  /** Validate if all required answers has been provided and the claim is ready to produce output. */
  override implicit val validator: Validate[RejectedGoodsMultipleClaim] =
    all(
      hasMRNAndImportDeclaration,
      containsOnlySupportedTaxCodes,
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
      supportingEvidenceHasBeenProvided,
      declarationsHasNoSubsidyPayments,
      payeeTypeIsDefined
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
  implicit val format: Format[RejectedGoodsMultipleClaim] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new RejectedGoodsMultipleClaim(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(claim =>
        (claim.answers, claim.startTimeSeconds, claim.caseNumber, claim.submissionDateTime, claim.features)
      )
    )

  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, RejectedGoodsMultipleClaim] =
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
      .flatMapWhenDefined(answers.basisOfClaimSpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails
      )
      .mapWhenDefined(answers.methodOfDisposal)(_.submitMethodOfDisposal)
      .mapWhenDefined(answers.detailsOfRejectedGoods)(_.submitDetailsOfRejectedGoods)
      .flatMapEachWhenDefined(answers.correctedAmounts)(j => {
        case (mrn: MRN, reimbursements: Map[TaxCode, Option[BigDecimal]]) =>
          j.selectAndReplaceTaxCodeSetForReimbursement(mrn, reimbursements.keySet.toSeq)
            .flatMapEachWhenMappingDefined(reimbursements)(j =>
              (taxCode, amount) => j.submitCorrectAmount(mrn, taxCode, amount)
            )
      })
      .mapWhenDefined(answers.inspectionDate)(_.submitInspectionDate)
      .mapWhenDefined(answers.inspectionAddress)(_.submitInspectionAddress)
      .flatMapWhenDefined(answers.payeeType)(_.submitPayeeType)
      .flatMapWhenDefined(answers.bankAccountDetails)(_.submitBankAccountDetails)
      .flatMapWhenDefined(answers.bankAccountType)(_.submitBankAccountType)
      .flatMapEach(
        answers.supportingEvidences,
        j =>
          (e: UploadedFile) =>
            j.receiveUploadedFiles(e.documentType.orElse(Some(UploadDocumentType.Other)), answers.nonce, Seq(e))
      )
      .map(_.submitCheckYourAnswersChangeMode(answers.checkYourAnswersChangeMode))

  /** This method MUST BE used only to test the validation correctness of the invalid answer states. */
  def unsafeModifyAnswers(
    claim: RejectedGoodsMultipleClaim,
    f: RejectedGoodsMultipleClaim.Answers => RejectedGoodsMultipleClaim.Answers
  ): RejectedGoodsMultipleClaim =
    RejectedGoodsMultipleClaim(
      answers = f(claim.answers),
      startTimeSeconds = claim.startTimeSeconds,
      features = claim.features
    )
}
