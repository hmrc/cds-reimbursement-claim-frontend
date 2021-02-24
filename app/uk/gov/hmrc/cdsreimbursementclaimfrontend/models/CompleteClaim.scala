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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import cats.syntax.eq._
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimantDetailsAsImporterCompanyController.ClaimantDetailsAsImporterCompany
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimantDetailsAsIndividualController.ClaimantDetailsAsIndividual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarantEoriNumberController.DeclarantEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDutyAmountsController.{EnterClaim, EnterEuClaim}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterImporterEoriNumberController.ImporterEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectReasonForBasisAndClaimController.SelectReasonForClaimAndBasis
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{BankAccountController, SelectWhoIsMakingTheClaimController}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswers.CompleteBankAccountDetailAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimAnswers.CompleteClaimAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantDetailsAsIndividualAnswer.CompleteClaimantDetailsAsIndividualAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CommoditiesDetailsAnswers.CompleteCommodityDetailsAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.CompleteDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EitherUtils._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers

import java.util.UUID

sealed trait CompleteClaim extends Product with Serializable {
  val id: UUID
}

final case class CompleteC285Claim(
  id: UUID,
  movementReferenceNumber: Either[EntryNumber, MRN],
  duplicateMovementReferenceNumberAnswer: Option[Either[EntryNumber, MRN]],
  declarationDetails: Option[DeclarationDetailAnswers],
  duplicateDeclarationDetails: Option[EntryDeclarationDetails],
  declarantType: CompleteDeclarantTypeAnswer,
  claimantDetailsAsIndividualAnswer: CompleteClaimantDetailsAsIndividualAnswer,
  claimantDetailsAsImporterCompanyAnswer: Option[ClaimantDetailsAsImporterCompanyAnswer],
  bankAccountDetails: CompleteBankAccountDetailAnswers,
  supportingEvidenceAnswers: CompleteSupportingEvidenceAnswers,
  maybeUKDuty: Option[EnterClaim],
  maybeEUDuty: Option[EnterEuClaim],
  claims: CompleteClaimAnswers,
  commodityDetails: CompleteCommodityDetailsAnswers,
  maybeReasonForClaimAndBasisAnswer: Option[SelectReasonForClaimAndBasis],
  maybeReasonForClaim: Option[BasisForClaim],
  maybeDisplayDeclaration: Option[DisplayDeclaration],
  maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration],
  importerEoriNumber: Option[ImporterEoriNumber],
  declarantEoriNumber: Option[DeclarantEoriNumber]
) extends CompleteClaim

object CompleteC285Claim {

  def fromDraftClaim(draftClaim: DraftClaim): Option[CompleteC285Claim] =
    draftClaim match {
      case DraftClaim.DraftC285Claim(
            id,
            Some(completeMovementReferenceNumberAnswer: CompleteMovementReferenceNumberAnswer),
            duplicateMovementReferenceNumberAnswer,
            declarationDetailAnswers,
            duplicateDeclarationDetailAnswers,
            Some(completeDeclarantTypeAnswer: CompleteDeclarantTypeAnswer),
            Some(completeClaimantDetailsAsIndividualAnswer: CompleteClaimantDetailsAsIndividualAnswer),
            claimantDetailsAsImporterCompanyAnswers,
            Some(completeBankAccountDetailAnswers: CompleteBankAccountDetailAnswers),
            reasonForClaim,
            Some(s: CompleteSupportingEvidenceAnswers),
            ukDutyAmountAnswers,
            euDutyAmountAnswers,
            Some(completeClaimAnswers: CompleteClaimAnswers),
            Some(completeCommodityDetailsAnswers: CompleteCommodityDetailsAnswers),
            reasonForBasisAndClaimAnswer,
            maybeDisplayDeclaration,
            maybeDuplicateDisplayDeclaration,
            importerEoriNumberAnswer,
            declarantEoriNumberAnswer
          ) =>
        completeMovementReferenceNumberAnswer.movementReferenceNumber match {
          case Left(entryNumber) =>
            Some(
              CompleteC285Claim(
                id = id,
                Left(entryNumber),
                duplicateMovementReferenceNumberAnswer.flatMap(duplicateMovementReferenceNumberAnswer =>
                  duplicateMovementReferenceNumberAnswer.maybeDuplicateMovementReferenceNumber
                ),
                declarationDetailAnswers,
                duplicateDeclarationDetailAnswers.flatMap(duplicateDeclarantDetailAnswers =>
                  duplicateDeclarantDetailAnswers.duplicateDeclaration
                ),
                completeDeclarantTypeAnswer,
                completeClaimantDetailsAsIndividualAnswer,
                claimantDetailsAsImporterCompanyAnswers,
                completeBankAccountDetailAnswers,
                supportingEvidenceAnswers = s,
                ukDutyAmountAnswers.flatMap(p => p.maybeUkDuty),
                euDutyAmountAnswers.flatMap(p => p.maybeEuDuty),
                completeClaimAnswers,
                completeCommodityDetailsAnswers,
                reasonForBasisAndClaimAnswer.flatMap(rcb => rcb.reasonForClaimAndBasis),
                reasonForClaim.flatMap(p => p.reason),
                maybeDisplayDeclaration = None,
                maybeDuplicateDisplayDeclaration = None,
                None,
                None
              )
            )
          case Right(mrn)        =>
            Some(
              CompleteC285Claim(
                id = id,
                Right(mrn),
                duplicateMovementReferenceNumberAnswer.flatMap(duplicateMovementReferenceNumberAnswer =>
                  duplicateMovementReferenceNumberAnswer.maybeDuplicateMovementReferenceNumber
                ),
                None,
                duplicateDeclarationDetailAnswers.flatMap(duplicateDeclarantDetailAnswers =>
                  duplicateDeclarantDetailAnswers.duplicateDeclaration
                ),
                completeDeclarantTypeAnswer,
                completeClaimantDetailsAsIndividualAnswer,
                claimantDetailsAsImporterCompanyAnswers,
                completeBankAccountDetailAnswers,
                supportingEvidenceAnswers = s,
                ukDutyAmountAnswers.flatMap(p => p.maybeUkDuty),
                euDutyAmountAnswers.flatMap(p => p.maybeEuDuty),
                completeClaimAnswers,
                completeCommodityDetailsAnswers,
                None,
                reasonForClaim.flatMap(p => p.reason),
                maybeDisplayDeclaration,
                maybeDuplicateDisplayDeclaration,
                importerEoriNumberAnswer.flatMap(ie => ie.importerEori),
                declarantEoriNumberAnswer.flatMap(de => de.declarantEori)
              )
            )
        }
      case _ => None
    }

  implicit val eq: Eq[CompleteC285Claim]          = Eq.fromUniversalEquals[CompleteC285Claim]
  implicit val format: OFormat[CompleteC285Claim] = Json.format[CompleteC285Claim]
}

object CompleteClaim {

  implicit class CompleteClaimOps(private val completeClaim: CompleteClaim) {

    def maybeDisplayDeclaration: Option[DisplayDeclaration] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            maybeDisplayDeclaration,
            _,
            _,
            _
          ) =>
        maybeDisplayDeclaration
    }

    def maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            maybeDisplayDeclaration,
            _,
            _
          ) =>
        maybeDisplayDeclaration
    }

    def duplicateEntryDeclarationDetails: Option[EntryDeclarationDetails] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            duplicateDeclarationDetails,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        duplicateDeclarationDetails
    }

    def entryDeclarationDetails: Option[EntryDeclarationDetails] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            declarationDetailAnswers,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        declarationDetailAnswers match {
          case Some(value) =>
            value match {
              case DeclarationDetailAnswers.IncompleteDeclarationDetailAnswer(declarationDetails) => declarationDetails
              case DeclarationDetailAnswers.CompleteDeclarationDetailAnswer(declarationDetails)   =>
                Some(declarationDetails)
            }
          case None        => None
        }
    }

    def declarantType: SelectWhoIsMakingTheClaimController.DeclarantType = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            declarantType,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        declarantType.declarantType
    }

    def reasonForClaim: Either[SelectReasonForClaimAndBasis, BasisForClaim] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            maybeReasonForClaimAndBasisAnswer,
            maybeReasonForClaim,
            _,
            _,
            _,
            _
          ) =>
        (maybeReasonForClaimAndBasisAnswer, maybeReasonForClaim) match {
          case (Some(rc), None) => Left(rc)
          case (None, Some(r))  => Right(r)
          case _                => sys.error("invalid state: cannot have both reason-for-claim-and-basis and reason-for-claim")
        }
    }

    def claimantDetailsAsImporterCompany: Option[ClaimantDetailsAsImporterCompany] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            claimantDetailsAsImporterCompanyAnswer,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        claimantDetailsAsImporterCompanyAnswer match {
          case Some(value) =>
            value match {
              case ClaimantDetailsAsImporterCompanyAnswer.IncompleteClaimantDetailsAsImporterCompanyAnswer(
                    claimantDetailsAsImporterCompany
                  ) =>
                claimantDetailsAsImporterCompany
              case ClaimantDetailsAsImporterCompanyAnswer.CompleteClaimantDetailsAsImporterCompanyAnswer(
                    claimantDetailsAsImporterCompany
                  ) =>
                Some(claimantDetailsAsImporterCompany)
            }
          case None        => None
        }
    }

    def claimantDetailsAsIndividual: ClaimantDetailsAsIndividual = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            claimantDetailsAsIndividualAnswer,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        claimantDetailsAsIndividualAnswer.claimantDetailsAsIndividual
    }

    def commodityDetails: String = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            commodityDetails,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        commodityDetails.commodityDetails.value
    }

    def bankDetails: BankAccountController.BankAccountDetails = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            bankAccountDetails,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        bankAccountDetails.bankAccountDetails
    }

    def bankAccountType: String = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            bankAccountDetails,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        bankAccountDetails.bankAccountDetails.isBusinessAccount.headOption match {
          case Some(value) => if (value === 0) "Business Account" else "Non-Business Account"
          case None        => ""
        }
    }

    def totalUkDutyToClaim: BigDecimal = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            ukDuty,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        ukDuty match {
          case Some(value) => BigDecimal(value.dutyAmounts.map(f => f.claim.getOrElse(BigDecimal(0)).toDouble).sum)
          case None        => BigDecimal(0)
        }
    }

    def totalEuDutyToClaim: BigDecimal = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            euDuty,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        euDuty match {
          case Some(value) => BigDecimal(value.dutyAmounts.map(f => f.claim.getOrElse(BigDecimal(0)).toDouble).sum)
          case None        => BigDecimal(0)
        }
    }

    def movementReferenceNumber: Either[EntryNumber, MRN] = completeClaim match {
      case CompleteC285Claim(
            _,
            movementReferenceNumber,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        movementReferenceNumber
    }

    def duplicateMovementReferenceNumber: Either[EntryNumber, MRN] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            duplicateMovementReferenceNumberAnswer,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        duplicateMovementReferenceNumberAnswer match {
          case Some(value) => value
          case None        => sys.error("could not find movement reference number")
        }
    }

    def supportingEvidences: List[SupportingEvidence] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            supportingEvidenceAnswers,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        supportingEvidenceAnswers.evidences
    }

  }

  implicit val eq: Eq[CompleteClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[CompleteClaim] = derived.oformat()

}
