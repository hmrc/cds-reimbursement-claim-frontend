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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OptionUtils._

import java.time.LocalDate

import RejectedGoodsSingleJourney.Answers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference

// This file contains complete data model of the rejected-goods single journey.

object RejectedGoodsSingleJourney {

  def empty: RejectedGoodsSingleJourney =
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
    reimbursementMethodAnswer: Option[ReimbursementMethodAnswer] = None,
    supportingEvidences: Option[Map[UploadDocument, Option[DocumentTypeRejectedGoods]]] = None
  ) {

    def isComplete: Boolean =
      all(
        movementReferenceNumber,
        displayDeclaration,
        allOrNone(importerEoriNumber, declarantEoriNumber),
        declarantType,
        allOrNone(contactDetails, contactAddress),
        basisOfClaim,
        requiredWhen(basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances))(
          basisOfClaimSpecialCircumstances
        ),
        detailsOfRejectedGoods,
        nonEmptyMap(reimbursementClaims),
        allOrNone(inspectionDate, inspectionAddress),
        allOrNone(bankAccountDetails, bankAccountType),
        requiredWhen(isAllSelectedDutiesAreCMAEligible)(reimbursementMethodAnswer),
        nonEmptyMap(supportingEvidences)
      ).isDefined

    def getNdrcDetails: Option[List[NdrcDetails]] =
      for {
        acc14           <- displayDeclaration
        ndrcDetailsList <- acc14.displayResponseDetail.ndrcDetails
      } yield ndrcDetailsList

    def getNdrcDetailsFor(taxCode: TaxCode): Option[NdrcDetails] =
      for {
        ndrcDetailsList <- getNdrcDetails
        ndrcDetail      <- ndrcDetailsList.find(_.taxType === taxCode.value)
      } yield ndrcDetail

    def getSelectedDuties: Option[Seq[TaxCode]] =
      reimbursementClaims.map(_.keys.toSeq)

    def isAllSelectedDutiesAreCMAEligible: Boolean =
      reimbursementClaims
        .map(_.keySet.map(getNdrcDetailsFor).collect { case Some(d) => d })
        .exists(_.forall(_.cmaEligible.isDefined))

  }

  object Answers {
    implicit lazy val mapFormat1: Format[Map[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatWithOptionalValue[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[Map[UploadDocument, Option[DocumentTypeRejectedGoods]]] =
      MapFormat.formatWithOptionalValue[UploadDocument, DocumentTypeRejectedGoods]

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.format[Answers]
  }

  implicit val format: Format[RejectedGoodsSingleJourney] =
    Format(
      Reads(Answers.format.reads(_).map(answers => new RejectedGoodsSingleJourney(answers))),
      Writes(journey => Answers.format.writes(journey.answers))
    )

  implicit val equality: Eq[RejectedGoodsSingleJourney] =
    Eq.fromUniversalEquals[RejectedGoodsSingleJourney]

}

// Encapsulated C&E1179 single MRN journey logic
final class RejectedGoodsSingleJourney private (val answers: Answers) {

  def isComplete: Boolean = answers.isComplete

  // resets the journey with the new MRN
  def submitMovementReferenceNumber(mrn: MRN): RejectedGoodsSingleJourney =
    answers.movementReferenceNumber match {
      case Some(existing) if existing === mrn => this
      case _                                  =>
        new RejectedGoodsSingleJourney(Answers(movementReferenceNumber = Some(mrn)))
    }

  // sets the ACC14 declaration and resets all reimbursementClaims
  def submitDisplayDeclaration(displayDeclaration: DisplayDeclaration): RejectedGoodsSingleJourney =
    answers.displayDeclaration match {
      case Some(existing) if existing === displayDeclaration => this
      case _                                                 =>
        new RejectedGoodsSingleJourney(
          answers.copy(displayDeclaration = Some(displayDeclaration), reimbursementClaims = None)
        )
    }

  def submitImporterEoriNumber(importerEoriNumber: Eori): RejectedGoodsSingleJourney =
    answers.importerEoriNumber match {
      case Some(existing) if existing === importerEoriNumber => this
      case _                                                 =>
        new RejectedGoodsSingleJourney(
          answers.copy(importerEoriNumber = Some(importerEoriNumber))
        )
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): RejectedGoodsSingleJourney =
    answers.declarantEoriNumber match {
      case Some(existing) if existing === declarantEoriNumber => this
      case _                                                  =>
        new RejectedGoodsSingleJourney(
          answers.copy(declarantEoriNumber = Some(declarantEoriNumber))
        )
    }

  def submitDeclarantType(declarantType: DeclarantTypeAnswer): RejectedGoodsSingleJourney =
    answers.declarantType match {
      case Some(existing) if existing === declarantType => this
      case _                                            =>
        new RejectedGoodsSingleJourney(
          answers.copy(declarantType = Some(declarantType))
        )
    }

  def submitContactDetails(contactDetails: MrnContactDetails): RejectedGoodsSingleJourney =
    answers.contactDetails match {
      case Some(existing) if existing === contactDetails => this
      case _                                             =>
        new RejectedGoodsSingleJourney(
          answers.copy(contactDetails = Some(contactDetails))
        )
    }

  def submitContactAddress(contactAddress: ContactAddress): RejectedGoodsSingleJourney =
    answers.contactAddress match {
      case Some(existing) if existing === contactAddress => this
      case _                                             =>
        new RejectedGoodsSingleJourney(
          answers.copy(contactAddress = Some(contactAddress))
        )
    }

  def submitBasisOfClaim(basisOfClaim: BasisOfRejectedGoodsClaim): RejectedGoodsSingleJourney =
    answers.basisOfClaim match {
      case Some(existing) if existing === basisOfClaim => this
      case _                                           =>
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

  def submitBasisOfClaimSpecialCircumstances(
    basisOfClaimSpecialCircumstances: String
  ): Either[String, RejectedGoodsSingleJourney] =
    answers.basisOfClaim match {
      case Some(BasisOfRejectedGoodsClaim.SpecialCircumstances) =>
        Right(
          new RejectedGoodsSingleJourney(
            answers.copy(basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstances))
          )
        )

      case _ => Left("basisOfClaim.not_matching")
    }

  // overwrites basisOfClaim with SpecialCircumstances enum value
  def forceSubmitBasisOfClaimSpecialCircumstances(
    basisOfClaimSpecialCircumstances: String
  ): RejectedGoodsSingleJourney =
    answers.basisOfClaimSpecialCircumstances match {
      case Some(existing) if existing === basisOfClaimSpecialCircumstances => this
      case _                                                               =>
        new RejectedGoodsSingleJourney(
          answers.copy(
            basisOfClaim = Some(BasisOfRejectedGoodsClaim.SpecialCircumstances),
            basisOfClaimSpecialCircumstances = Some(basisOfClaimSpecialCircumstances)
          )
        )
    }

  def submitMethodOfDisposal(methodOfDisposal: MethodOfDisposal): RejectedGoodsSingleJourney =
    answers.methodOfDisposal match {
      case Some(existing) if existing === methodOfDisposal => this
      case _                                               =>
        new RejectedGoodsSingleJourney(
          answers.copy(methodOfDisposal = Some(methodOfDisposal))
        )
    }

  def submitDetailsOfRejectedGoods(detailsOfRejectedGoods: String): RejectedGoodsSingleJourney =
    answers.detailsOfRejectedGoods match {
      case Some(existing) if existing === detailsOfRejectedGoods => this
      case _                                                     =>
        new RejectedGoodsSingleJourney(
          answers.copy(detailsOfRejectedGoods = Some(detailsOfRejectedGoods))
        )
    }

  def selectTaxCodeForReimbursement(taxCode: TaxCode): Either[String, RejectedGoodsSingleJourney] =
    answers.displayDeclaration match {
      case None => Left("selectTaxCodeForReimbursement.missingDisplayDeclaration")

      case Some(_) =>
        if (answers.getNdrcDetailsFor(taxCode).isDefined) {
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

  def selectAndReplaceTaxCodeSetForReimbursement(taxCodeSet: Set[TaxCode]): Either[String, RejectedGoodsSingleJourney] =
    answers.displayDeclaration match {
      case None => Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")

      case Some(_) =>
        val allTaxCodesExistInACC14 = taxCodeSet.forall(answers.getNdrcDetailsFor(_).isDefined)
        if (allTaxCodesExistInACC14) {
          val newReimbursementClaims = answers.reimbursementClaims match {
            case None                      =>
              taxCodeSet.map(taxCode => (taxCode -> None)).toMap

            case Some(reimbursementClaims) =>
              taxCodeSet.map { taxCode =>
                taxCode -> reimbursementClaims.get(taxCode).flatten
              }.toMap
          }
          Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))
        } else
          Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
    }

  def isValidCorrectedAmount(correctedAmount: BigDecimal, ndrcDetails: NdrcDetails): Boolean =
    correctedAmount >= 0 && correctedAmount < BigDecimal(ndrcDetails.amount)

  def submitCorrectedAmountForReimbursement(
    taxCode: TaxCode,
    correctedAmount: BigDecimal
  ): Either[String, RejectedGoodsSingleJourney] =
    answers.displayDeclaration match {
      case None =>
        Left("submitCorrectedAmountForReimbursement.missingDisplayDeclaration")

      case Some(_) =>
        answers.getNdrcDetailsFor(taxCode) match {
          case None =>
            Left("submitCorrectedAmountForReimbursement.taxCodeNotInACC14")

          case Some(ndrcDetails) if isValidCorrectedAmount(correctedAmount, ndrcDetails) =>
            val newReimbursementClaims = answers.reimbursementClaims match {
              case None                      => Map(taxCode -> Some(correctedAmount))
              case Some(reimbursementClaims) =>
                reimbursementClaims.get(taxCode) match {
                  case None => reimbursementClaims + (taxCode -> Some(correctedAmount))
                  case _    => reimbursementClaims
                }
            }
            Right(new RejectedGoodsSingleJourney(answers.copy(reimbursementClaims = Some(newReimbursementClaims))))

          case _ =>
            Left("submitCorrectedAmountForReimbursement.invalidAmount")
        }
    }

  implicit val equalityOfLocalDate: Eq[LocalDate] = Eq.fromUniversalEquals[LocalDate]

  def submitInspectionDate(inspectionDate: LocalDate): Either[String, RejectedGoodsSingleJourney] =
    if (inspectionDate.isAfter(LocalDate.now()))
      Right(answers.inspectionDate match {
        case Some(existing) if existing === inspectionDate => this
        case _                                             =>
          new RejectedGoodsSingleJourney(
            answers.copy(inspectionDate = Some(inspectionDate))
          )
      })
    else
      Left("submitInspectionDate.mustBeFutureDate")

  def submitInspectionAddress(inspectionAddress: InspectionAddress): RejectedGoodsSingleJourney =
    answers.inspectionAddress match {
      case Some(existing) if existing === inspectionAddress => this
      case _                                                =>
        new RejectedGoodsSingleJourney(
          answers.copy(inspectionAddress = Some(inspectionAddress))
        )
    }

  def submitBankAccountDetails(bankAccountDetails: BankAccountDetails): RejectedGoodsSingleJourney =
    answers.bankAccountDetails match {
      case Some(existing) if existing === bankAccountDetails => this
      case _                                                 =>
        new RejectedGoodsSingleJourney(
          answers.copy(bankAccountDetails = Some(bankAccountDetails))
        )
    }

  def submitBankAccountType(bankAccountType: BankAccountType): RejectedGoodsSingleJourney =
    answers.bankAccountType match {
      case Some(existing) if existing === bankAccountType => this
      case _                                              =>
        new RejectedGoodsSingleJourney(
          answers.copy(bankAccountType = Some(bankAccountType))
        )
    }

  def submitReimbursementMethodAnswer(
    reimbursementMethodAnswer: ReimbursementMethodAnswer
  ): Either[String, RejectedGoodsSingleJourney] =
    if (
      reimbursementMethodAnswer === ReimbursementMethodAnswer.BankAccountTransfer ||
      answers.isAllSelectedDutiesAreCMAEligible
    )
      Right(answers.reimbursementMethodAnswer match {
        case Some(existing) if existing === reimbursementMethodAnswer => this
        case _                                                        =>
          new RejectedGoodsSingleJourney(
            answers.copy(reimbursementMethodAnswer = Some(reimbursementMethodAnswer))
          )
      })
    else
      Left("submitReimbursementMethodAnswer.notCMAEligible")

  def submitUploadDocument(uploadDocument: UploadDocument): RejectedGoodsSingleJourney =
    answers.supportingEvidences match {
      case Some(supportingEvidences) if supportingEvidences.contains(uploadDocument) =>
        this

      case Some(supportingEvidences) =>
        new RejectedGoodsSingleJourney(
          answers.copy(supportingEvidences = Some(supportingEvidences + (uploadDocument -> None)))
        )

      case None =>
        new RejectedGoodsSingleJourney(answers.copy(supportingEvidences = Some(Map(uploadDocument -> None))))
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

  override def hashCode(): Int = answers.hashCode

}
