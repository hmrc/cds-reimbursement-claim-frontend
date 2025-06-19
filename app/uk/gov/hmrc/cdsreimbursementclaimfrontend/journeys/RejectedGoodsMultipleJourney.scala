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
import cats.syntax.eq.*
import com.github.arturopala.validator.Validator
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.*

import java.time.LocalDate
import java.time.LocalDateTime
import scala.collection.immutable.SortedMap
import java.time.Instant

/** An encapsulated C&E1179 multiple MRN journey logic. The constructor of this class MUST stay PRIVATE to protected
  * integrity of the journey.
  *
  * The journey uses two nested case classes:
  *
  *   - [[RejectedGoodsMultipleJourney.Answers]] - keeps record of user answers and acquired documents
  *   - [[RejectedGoodsMultipleJourney.Output]] - final outcome of the journey to be sent to backend processing
  */
final class RejectedGoodsMultipleJourney private (
  val answers: RejectedGoodsMultipleJourney.Answers,
  val startTimeSeconds: Long,
  val caseNumber: Option[String] = None,
  val submissionDateTime: Option[LocalDateTime] = None,
  val features: Option[RejectedGoodsMultipleJourney.Features]
) extends JourneyBase
    with DirectFluentSyntax[RejectedGoodsMultipleJourney]
    with RejectedGoodsJourneyProperties
    with HaveInspectionDetails
    with JourneyAnalytics {

  type Type = RejectedGoodsMultipleJourney

  val self: RejectedGoodsMultipleJourney = this

  val validate: Validator.Validate[RejectedGoodsMultipleJourney] =
    RejectedGoodsMultipleJourney.validator

  private def copy(
    newAnswers: RejectedGoodsMultipleJourney.Answers
  ): RejectedGoodsMultipleJourney =
    new RejectedGoodsMultipleJourney(newAnswers, startTimeSeconds, caseNumber, submissionDateTime, features)

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

  def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    getLeadMovementReferenceNumber.flatMap(getDisplayDeclarationFor)

  def getNthDisplayDeclaration(index: Int): Option[DisplayDeclaration] =
    getNthMovementReferenceNumber(index).flatMap(getDisplayDeclarationFor)

  def getDisplayDeclarationFor(mrn: MRN): Option[DisplayDeclaration] =
    for
      declarations <- answers.displayDeclarations
      declaration  <- declarations.find(_.getMRN === mrn)
    yield declaration

  override def getDisplayDeclarations: Seq[DisplayDeclaration] =
    answers.displayDeclarations.getOrElse(Seq.empty)

  def getCorrectAmountsFor(mrn: MRN): Option[OrderedMap[TaxCode, Option[BigDecimal]]] =
    answers.correctedAmounts.flatMap(_.get(mrn))

  def getCorrectedAmountFor(mrn: MRN, taxCode: TaxCode): Option[BigDecimal] =
    for
      all           <- answers.correctedAmounts
      thisMrn       <- all.get(mrn)
      correctAmount <- thisMrn.get(taxCode).flatten
    yield correctAmount

  def getAvailableTaxCodesWithPaidAmountsFor(declarationId: MRN): Seq[(TaxCode, BigDecimal)] =
    getDisplayDeclarationFor(declarationId)
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

  def needsBanksAccountDetailsSubmission: Boolean =
    !this.isSubsidyOnlyJourney

  def needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex: Int): Boolean =
    if pageIndex === 1 then needsDeclarantAndConsigneeEoriSubmission else false

  def getNdrcDetailsFor(mrn: MRN): Option[List[NdrcDetails]] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsList)

  def getNdrcDetailsFor(mrn: MRN, taxCode: TaxCode): Option[NdrcDetails] =
    getDisplayDeclarationFor(mrn).flatMap(_.getNdrcDetailsFor(taxCode.value))

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

  def withDutiesChangeMode(enabled: Boolean): RejectedGoodsMultipleJourney =
    this.copy(answers.copy(modes = answers.modes.copy(dutiesChangeMode = enabled)))

  def withEnterContactDetailsMode(enabled: Boolean): RejectedGoodsMultipleJourney =
    this.copy(answers.copy(modes = answers.modes.copy(enterContactDetailsMode = enabled)))

  override def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]] =
    Some(UploadDocumentType.rejectedGoodsMultipleDocumentTypes)

  def isPaymentMethodsMatching(displayDeclaration: DisplayDeclaration): Boolean =
    getLeadDisplayDeclaration
      .flatMap(leadDisplayDeclaration => leadDisplayDeclaration.getNdrcDetailsList)
      .fold {
        false
      } { (leadNdrcDetails: List[NdrcDetails]) =>
        displayDeclaration.getNdrcDetailsList.fold {
          false
        } { (ndrcDetails: List[NdrcDetails]) =>
          val paymentMethodsFromDisplayDeclaration: List[String] = ndrcDetails.map(_.paymentMethod).distinct
          val leadPaymentMethods: List[String]                   = leadNdrcDetails.map(_.paymentMethod).distinct
          (leadPaymentMethods, paymentMethodsFromDisplayDeclaration) match {
            case (Seq("006"), Seq("006"))                           => true
            case (a, b) if !a.contains("006") && !b.contains("006") => true
            case _                                                  => false
          }
        }
      }

  def getSubsidyError: String =
    getLeadDisplayDeclaration
      .flatMap(leadDisplayDeclaration => leadDisplayDeclaration.getNdrcDetailsList)
      .fold {
        "submitMovementReferenceNumber.needsNonSubsidy"
      } { (leadNdrcDetails: List[NdrcDetails]) =>
        leadNdrcDetails.map(_.paymentMethod).distinct match {
          case a if a.contains("006") => "submitMovementReferenceNumber.needsSubsidy"
          case _                      => "submitMovementReferenceNumber.needsNonSubsidy"
        }
      }

  def containsUnsupportedTaxCodeFor(mrn: MRN): Boolean =
    getDisplayDeclarationFor(mrn).exists(_.containsSomeUnsupportedTaxCode)

  def removeUnsupportedTaxCodes(): RejectedGoodsMultipleJourney =
    this.copy(answers.copy(displayDeclarations = answers.displayDeclarations.map(_.map(_.removeUnsupportedTaxCodes()))))

  def submitMovementReferenceNumberAndDeclaration(
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ): Either[String, RejectedGoodsMultipleJourney] =
    submitMovementReferenceNumberAndDeclaration(0, mrn, displayDeclaration)

  def submitMovementReferenceNumberAndDeclaration(
    index: Int,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration
  ) =
    whileClaimIsAmendable {
      if index < 0 then Left("submitMovementReferenceNumber.negativeIndex")
      else if index > countOfMovementReferenceNumbers then Left("submitMovementReferenceNumber.invalidIndex")
      else if mrn =!= displayDeclaration.getMRN then
        Left(
          "submitMovementReferenceNumber.wrongDisplayDeclarationMrn"
        )
      else if index > 0 && !getLeadDisplayDeclaration.exists(displayDeclaration.hasSameEoriAs) then
        Left("submitMovementReferenceNumber.wrongDisplayDeclarationEori")
      else if index > 0 && features.exists(_.shouldAllowSubsidyOnlyPayments) && !isPaymentMethodsMatching(
          displayDeclaration
        )
      then {
        Left(getSubsidyError)
      } else
        getNthMovementReferenceNumber(index) match {
          // do nothing if MRN value and positions does not change, and declaration is the same
          case Some(existingMrn)
              if existingMrn === mrn &&
                getDisplayDeclarationFor(mrn).contains(displayDeclaration) =>
            Right(this)

          // change an existing MRN
          case Some(existingMrn) =>
            if getIndexOfMovementReferenceNumber(mrn).exists(_ =!= index) then
              Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
            else if index === 0 then {
              // first MRN change resets all the journey
              Right(
                new RejectedGoodsMultipleJourney(
                  RejectedGoodsMultipleJourney
                    .Answers(
                      userEoriNumber = answers.userEoriNumber,
                      movementReferenceNumbers = Some(Seq(mrn)),
                      displayDeclarations = Some(Seq(displayDeclaration)),
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
                    displayDeclarations = answers.displayDeclarations.map(
                      _.filterNot(_.displayResponseDetail.declarationId === existingMrn.value) :+ displayDeclaration
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
                    displayDeclarations =
                      answers.displayDeclarations.map(_ :+ displayDeclaration).orElse(Some(Seq(displayDeclaration))),
                    correctedAmounts = answers.correctedAmounts
                      .map(_ + (mrn -> OrderedMap.empty[TaxCode, Option[BigDecimal]]))
                      .orElse(Some(OrderedMap(mrn -> OrderedMap.empty[TaxCode, Option[BigDecimal]])))
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
            this.copy(
              answers.copy(
                movementReferenceNumbers = answers.movementReferenceNumbers
                  .map(mrns => mrns.take(index) ++ mrns.drop(index + 1)),
                displayDeclarations = answers.displayDeclarations.map(
                  _.filterNot(_.displayResponseDetail.declarationId === mrn.value)
                ),
                correctedAmounts = answers.correctedAmounts.map(_.removed(mrn))
              )
            )
          )
      }
    }

  def submitUserXiEori(userXiEori: UserXiEori): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(eoriNumbersVerification =
          answers.eoriNumbersVerification
            .orElse(Some(EoriNumbersVerification()))
            .map(_.copy(userXiEori = Some(userXiEori)))
        )
      )
    }

  def submitConsigneeEoriNumber(consigneeEoriNumber: Eori): Either[String, RejectedGoodsMultipleJourney] =
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
            JourneyValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI
          )
      else Left("submitConsigneeEoriNumber.unexpected")
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): Either[String, RejectedGoodsMultipleJourney] =
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
        else Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
      else Left("submitDeclarantEoriNumber.unexpected")
    }

  def submitContactDetails(contactDetails: Option[MrnContactDetails]): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactDetails = contactDetails)
      )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(contactAddress = Some(contactAddress.computeChanges(getInitialAddressDetailsFromDeclaration)))
      )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsMultipleJourney =
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
  ): Either[String, RejectedGoodsMultipleJourney] =
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

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(methodOfDisposal = Some(methodOfDisposal))
      )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
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
          if taxCodes.isEmpty then Left("selectTaxCodeSetForReimbursement.emptySelection")
          else {
            val allTaxCodesExistInACC14 = taxCodes.forall(getNdrcDetailsFor(mrn, _).isDefined)
            if allTaxCodesExistInACC14 then {
              val newCorrectAmounts: RejectedGoodsMultipleJourney.CorrectedAmounts =
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
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      getDisplayDeclarationFor(declarationId) match {
        case None =>
          Left("submitCorrectAmount.missingDisplayDeclaration")

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
  ): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      getDisplayDeclarationFor(declarationId) match {
        case None =>
          Left("submitCorrectAmount.missingDisplayDeclaration")

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

  def submitInspectionDate(inspectionDate: InspectionDate): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionDate = Some(inspectionDate))
      )
    }

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(inspectionAddress = Some(inspectionAddress))
      )
    }

  def submitPayeeType(payeeType: PayeeType): Either[String, RejectedGoodsMultipleJourney] =
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

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      if needsBanksAccountDetailsSubmission then
        Right(
          this.copy(
            answers.copy(bankAccountDetails =
              Some(bankAccountDetails.computeChanges(getInitialBankAccountDetailsFromDeclaration))
            )
          )
        )
      else Left("submitBankAccountDetails.unexpected")
    }

  def removeBankAccountDetails(): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(
        answers.copy(bankAccountDetails = None)
      )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      if needsBanksAccountDetailsSubmission then
        Right(
          this.copy(
            answers.copy(bankAccountType = Some(bankAccountType))
          )
        )
      else Left("submitBankAccountType.unexpected")
    }

  def submitDocumentTypeSelection(documentType: UploadDocumentType): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      this.copy(answers.copy(selectedDocumentType = Some(documentType)))
    }

  def receiveUploadedFiles(
    documentType: Option[UploadDocumentType],
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, RejectedGoodsMultipleJourney] =
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

  def submitCheckYourAnswersChangeMode(enabled: Boolean): RejectedGoodsMultipleJourney =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          _ => this,
          _ => this.copy(answers.copy(modes = answers.modes.copy(checkYourAnswersChangeMode = enabled)))
        )
    }

  def finalizeJourneyWith(caseNumber: String): Either[String, RejectedGoodsMultipleJourney] =
    whileClaimIsAmendable {
      validate(this)
        .fold(
          errors => Left(errors.headMessage),
          _ =>
            Right(
              new RejectedGoodsMultipleJourney(
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
    if obj.isInstanceOf[RejectedGoodsMultipleJourney] then {
      val that = obj.asInstanceOf[RejectedGoodsMultipleJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int  = answers.hashCode
  override def toString: String = s"RejectedGoodsMultipleJourney${Json.prettyPrint(Json.toJson(this))}"

  /** Validates the journey and retrieves the output. */

  def toOutput: Either[Seq[String], RejectedGoodsMultipleJourney.Output] =
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
        yield RejectedGoodsMultipleJourney.Output(
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
          reimbursementMethod =
            if isSubsidyOnlyJourney then ReimbursementMethod.Subsidy
            else ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails =
            if isSubsidyOnlyJourney then None
            else answers.bankAccountDetails
        )).toRight(
          List("Unfortunately could not produce the output, please check if all answers are complete.")
        )
      )

}

object RejectedGoodsMultipleJourney extends JourneyCompanion[RejectedGoodsMultipleJourney] {

  /** A starting point to build new instance of the journey. */
  override def empty(
    userEoriNumber: Eori,
    nonce: Nonce = Nonce.random,
    features: Option[Features] = None
  ): RejectedGoodsMultipleJourney =
    new RejectedGoodsMultipleJourney(
      Answers(userEoriNumber = userEoriNumber, nonce = nonce),
      startTimeSeconds = Instant.now().getEpochSecond(),
      features = features
    )

  type CorrectedAmounts = OrderedMap[TaxCode, Option[BigDecimal]]

  final case class Features(
    shouldBlockSubsidies: Boolean,
    shouldAllowSubsidyOnlyPayments: Boolean
  ) extends SubsidiesFeatures

  // All user answers captured during C&E1179 multiple MRNs journey
  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumbers: Option[Seq[MRN]] = None,
    displayDeclarations: Option[Seq[DisplayDeclaration]] = None,
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
    modes: JourneyModes = JourneyModes()
  ) extends RejectedGoodsAnswers

  // Final minimal output of the journey we want to pass to the backend.
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
      whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments,
      payeeTypeIsDefined
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
  implicit val format: Format[RejectedGoodsMultipleJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "startTimeSeconds").read[Long]
        and (JsPath \ "caseNumber").readNullable[String]
        and (JsPath \ "submissionDateTime").readNullable[LocalDateTime]
        and (JsPath \ "features").readNullable[Features])(new RejectedGoodsMultipleJourney(_, _, _, _, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "startTimeSeconds").write[Long]
        and (JsPath \ "caseNumber").writeNullable[String]
        and (JsPath \ "submissionDateTime").writeNullable[LocalDateTime]
        and (JsPath \ "features").writeNullable[Features])(journey =>
        (journey.answers, journey.startTimeSeconds, journey.caseNumber, journey.submissionDateTime, journey.features)
      )
    )

  override def tryBuildFrom(
    answers: Answers,
    features: Option[Features] = None
  ): Either[String, RejectedGoodsMultipleJourney] =
    empty(answers.userEoriNumber, answers.nonce, features)
      .flatMapEachWhenDefined(answers.movementReferenceNumbers.zipOpt(answers.displayDeclarations).zipWithIndex)(j => {
        case ((mrn: MRN, decl: DisplayDeclaration), index: Int) =>
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
}
