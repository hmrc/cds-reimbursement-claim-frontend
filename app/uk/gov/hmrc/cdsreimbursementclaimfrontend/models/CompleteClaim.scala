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
import cats.data.Validated.Valid
import cats.implicits._
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.{AccountName, AccountNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterYourContactDetailsController.ContactDetailsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.ClaimantDetailsAsIndividual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectReasonForBasisAndClaimController.SelectReasonForClaimAndBasis
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{BankAccountController, SelectWhoIsMakingTheClaimController}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.CompleteBankAccountDetailAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaimAnswer.CompleteBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsAnswer.CompleteContactDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantDetailsAsIndividualAnswer.CompleteClaimantDetailsAsIndividualAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimsAnswer.CompleteClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CommoditiesDetailsAnswer.CompleteCommodityDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantEoriNumberAnswer.CompleteDeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.CompleteDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.CompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateDeclarationDetailsAnswer.CompleteDuplicateDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateMovementReferenceNumberAnswer.CompleteDuplicateMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ImporterEoriNumberAnswer.CompleteImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonAndBasisOfClaimAnswer.CompleteReasonAndBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswer.CompleteSupportingEvidenceAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{SupportingEvidence, SupportingEvidenceAnswer}

import java.util.UUID

sealed trait CompleteClaim extends Product with Serializable {
  val id: UUID
}

object CompleteClaim {

  final case class CompleteC285Claim(
    id: UUID,
    completeMovementReferenceNumberAnswer: CompleteMovementReferenceNumberAnswer,
    maybeCompleteDuplicateMovementReferenceNumberAnswer: Option[CompleteDuplicateMovementReferenceNumberAnswer],
    maybeCompleteDeclarationDetailsAnswer: Option[CompleteDeclarationDetailsAnswer],
    maybeCompleteDuplicateDeclarationDetailsAnswer: Option[CompleteDuplicateDeclarationDetailsAnswer],
    completeDeclarantTypeAnswer: CompleteDeclarantTypeAnswer,
    completeClaimantDetailsAsIndividualAnswer: CompleteClaimantDetailsAsIndividualAnswer,
    maybeClaimantDetailsAsImporterCompanyAnswer: Option[CompleteContactDetailsAnswer],
    maybeBasisOfClaimAnswer: Option[CompleteBasisOfClaimAnswer],
    maybeCompleteBankAccountDetailAnswer: Option[CompleteBankAccountDetailAnswer],
    supportingEvidenceAnswers: CompleteSupportingEvidenceAnswer,
    completeCommodityDetailsAnswer: CompleteCommodityDetailsAnswer,
    maybeCompleteReasonAndBasisOfClaimAnswer: Option[CompleteReasonAndBasisOfClaimAnswer],
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration],
    importerEoriNumber: Option[CompleteImporterEoriNumberAnswer],
    declarantEoriNumber: Option[CompleteDeclarantEoriNumberAnswer],
    completeClaimsAnswer: CompleteClaimsAnswer
  ) extends CompleteClaim

  object CompleteC285Claim {

    def fromDraftClaim(draftClaim: DraftClaim): Either[Error, CompleteC285Claim] =
      draftClaim match {
        case DraftClaim.DraftC285Claim(
              id,
              Some(draftCompleteMovementReferenceNumberAnswer: CompleteMovementReferenceNumberAnswer),
              draftMaybeDuplicateCompleteMovementReferenceNumberAnswer,
              draftMaybeDeclarationDetailsAnswer,
              draftDuplicateDeclarationDetailAnswer,
              draftDeclarantTypeAnswer,
              draftClaimantDetailsAsIndividualAnswer,
              draftClaimantDetailsAsImporterCompanyAnswer,
              draftBankAccountDetailAnswer,
              draftBasisForClaim,
              draftSupportingEvidence,
              _,
              draftCommodityAnswer,
              draftReasonAndBasisOfClaimAnswer,
              maybeDisplayDeclaration,
              maybeDuplicateDisplayDeclaration,
              draftImporterEoriNumberAnswer,
              draftDeclarantEoriNumberAnswer,
              Some(completeClaimsAnswer: CompleteClaimsAnswer)
            ) =>
          draftCompleteMovementReferenceNumberAnswer.movementReferenceNumber match {
            case Left(_) =>
              (
                validateDuplicateMovementReferenceNumberAnswer(
                  draftMaybeDuplicateCompleteMovementReferenceNumberAnswer
                ),
                validateDeclarationDetailsAnswer(draftMaybeDeclarationDetailsAnswer),
                validateDuplicateDeclarantDetailAnswer(draftDuplicateDeclarationDetailAnswer),
                validateDeclarantTypeAnswer(draftDeclarantTypeAnswer),
                validateClaimantDetailsAsIndividualAnswer(draftClaimantDetailsAsIndividualAnswer),
                validateClaimantDetailsAsImporterAnswer(draftClaimantDetailsAsImporterCompanyAnswer),
                validateBankAccountDetailAnswer(draftBankAccountDetailAnswer),
                validateSupportingEvidenceAnswer(draftSupportingEvidence),
                validateCommodityDetailsAnswer(draftCommodityAnswer),
                validateReasonAndBasisOfClaimAnswer(draftReasonAndBasisOfClaimAnswer),
                validateBasisOfClaimAnswer(draftBasisForClaim)
              )
                .mapN {
                  case (
                        completeMaybeDuplicateEntryReferenceNumberAnswer,
                        completeDeclarationDetailsAnswer,
                        completeDuplicateDeclarationDetailAnswer,
                        completeDeclarantTypeAnswer,
                        completeClaimantDetailsAsIndividualAnswer,
                        completeClaimantDetailsAsImporterCompanyAnswer,
                        completeBankAccountDetailAnswer,
                        completeSupportingEvidenceAnswer,
                        completeCommodityDetailsAnswer,
                        completeReasonAndBasisOfClaimAnswer,
                        completeBasisOfClaimAnswer
                      ) =>
                    CompleteC285Claim(
                      id = id,
                      completeMovementReferenceNumberAnswer = draftCompleteMovementReferenceNumberAnswer,
                      maybeCompleteDuplicateMovementReferenceNumberAnswer =
                        completeMaybeDuplicateEntryReferenceNumberAnswer,
                      maybeCompleteDeclarationDetailsAnswer = Some(completeDeclarationDetailsAnswer),
                      maybeCompleteDuplicateDeclarationDetailsAnswer = Some(completeDuplicateDeclarationDetailAnswer),
                      completeDeclarantTypeAnswer,
                      completeClaimantDetailsAsIndividualAnswer,
                      completeClaimantDetailsAsImporterCompanyAnswer,
                      completeBasisOfClaimAnswer,
                      completeBankAccountDetailAnswer,
                      supportingEvidenceAnswers = completeSupportingEvidenceAnswer,
                      completeCommodityDetailsAnswer,
                      completeReasonAndBasisOfClaimAnswer,
                      maybeDisplayDeclaration = None,
                      maybeDuplicateDisplayDeclaration = None,
                      None,
                      None,
                      completeClaimsAnswer
                    )
                }
                .toEither
                .leftMap { errors =>
                  Error(
                    s"could not create complete claim in order to submit claim request: ${errors.toList.mkString("; ")}"
                  )
                }

            case Right(_) =>
              (
                validateDuplicateMovementReferenceNumberAnswer(
                  draftMaybeDuplicateCompleteMovementReferenceNumberAnswer
                ),
                validateDeclarantTypeAnswer(draftDeclarantTypeAnswer),
                validateClaimantDetailsAsIndividualAnswer(draftClaimantDetailsAsIndividualAnswer),
                validateClaimantDetailsAsImporterAnswer(draftClaimantDetailsAsImporterCompanyAnswer),
                validateBankAccountDetailAnswer(draftBankAccountDetailAnswer),
                validateBasisOfClaimAnswer(draftBasisForClaim),
                validateSupportingEvidenceAnswer(draftSupportingEvidence),
                validateCommodityDetailsAnswer(draftCommodityAnswer),
                validateImporterEoriNumberAnswer(draftImporterEoriNumberAnswer),
                validateDeclarantEoriNumberAnswer(draftDeclarantEoriNumberAnswer)
              )
                .mapN {
                  case (
                        completeMaybeDuplicateMovementReferenceNumberAnswer,
                        completeDeclarantTypeAnswer,
                        completeClaimantDetailsAsIndividualAnswer,
                        completeClaimantDetailsAsImporterCompanyAnswer,
                        completeBankAccountDetailAnswer,
                        completeBasisOfClaimAnswer,
                        completeSupportingEvidenceAnswer,
                        completeCommodityDetailsAnswer,
                        completeImporterEoriNumberAnswer,
                        completeDeclarantEoriNumberAnswer
                      ) =>
                    CompleteC285Claim(
                      id = id,
                      completeMovementReferenceNumberAnswer = draftCompleteMovementReferenceNumberAnswer,
                      maybeCompleteDuplicateMovementReferenceNumberAnswer =
                        completeMaybeDuplicateMovementReferenceNumberAnswer,
                      maybeCompleteDeclarationDetailsAnswer = None,
                      maybeCompleteDuplicateDeclarationDetailsAnswer = None,
                      completeDeclarantTypeAnswer,
                      completeClaimantDetailsAsIndividualAnswer,
                      completeClaimantDetailsAsImporterCompanyAnswer,
                      completeBasisOfClaimAnswer,
                      completeBankAccountDetailAnswer,
                      completeSupportingEvidenceAnswer,
                      completeCommodityDetailsAnswer,
                      None,
                      maybeDisplayDeclaration,
                      maybeDuplicateDisplayDeclaration,
                      completeImporterEoriNumberAnswer,
                      completeDeclarantEoriNumberAnswer,
                      completeClaimsAnswer
                    )
                }
                .toEither
                .leftMap { errors =>
                  Error(
                    s"could not create complete claim in order to submit claim request: ${errors.toList.mkString("; ")}"
                  )
                }

          }
        case _ => Left(Error("unknown claim type"))
      }

    implicit val eq: Eq[CompleteC285Claim]          = Eq.fromUniversalEquals[CompleteC285Claim]
    implicit val format: OFormat[CompleteC285Claim] = Json.format[CompleteC285Claim]
  }

  def validateDeclarantEoriNumberAnswer(
    maybeDeclarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer]
  ): Validation[Option[CompleteDeclarantEoriNumberAnswer]] =
    maybeDeclarantEoriNumberAnswer match {
      case Some(value) =>
        value match {
          case DeclarantEoriNumberAnswer.IncompleteDeclarantEoriNumberAnswer(
                _
              ) =>
            invalid("incomplete declarant eori number answer")
          case completeDeclarantEoriNumberAnswer: CompleteDeclarantEoriNumberAnswer =>
            Valid(Some(completeDeclarantEoriNumberAnswer))
        }
      case None        => Valid(None)
    }

  def validateImporterEoriNumberAnswer(
    maybeImporterEoriNumberAnswer: Option[ImporterEoriNumberAnswer]
  ): Validation[Option[CompleteImporterEoriNumberAnswer]] =
    maybeImporterEoriNumberAnswer match {
      case Some(value) =>
        value match {
          case ImporterEoriNumberAnswer.IncompleteImporterEoriNumberAnswer(
                _
              ) =>
            invalid("incomplete eori number answer")
          case completeImporterEoriNumberAnswer: CompleteImporterEoriNumberAnswer =>
            Valid(Some(completeImporterEoriNumberAnswer))
        }
      case None        => Valid(None)
    }

  def validateReasonAndBasisOfClaimAnswer(
    maybeReasonAndBasisOfClaimAnswer: Option[ReasonAndBasisOfClaimAnswer]
  ): Validation[Option[CompleteReasonAndBasisOfClaimAnswer]] =
    maybeReasonAndBasisOfClaimAnswer match {
      case Some(value) =>
        value match {
          case ReasonAndBasisOfClaimAnswer.IncompleteReasonAndBasisOfClaimAnswer(
                _
              ) =>
            invalid("incomplete reason and basis of claim answer")
          case completeReasonAndBasisOfClaimAnswer: CompleteReasonAndBasisOfClaimAnswer =>
            Valid(Some(completeReasonAndBasisOfClaimAnswer))
        }
      case None        => Valid(None)
    }

  def validateBasisOfClaimAnswer(
    maybeBasisOfClaimAnswer: Option[BasisOfClaimAnswer],
    maybeReasonAndBasisOfClaimAnswer: Option[ReasonAndBasisOfClaimAnswer]
  ): Validation[Option[CompleteBasisOfClaimAnswer]] =
    maybeReasonAndBasisOfClaimAnswer match {
      case Some(_) => Valid(None)
      case None    =>
        maybeBasisOfClaimAnswer match {
          case Some(value) =>
            value match {
              case BasisOfClaimAnswer.IncompleteBasisOfClaimAnswer(maybeBasisOfClaim) =>
                maybeBasisOfClaim match {
                  case Some(value) => Valid(Some(CompleteBasisOfClaimAnswer(value)))
                  case None        => Valid(None)
                }
              case CompleteBasisOfClaimAnswer(basisOfClaim)                           => Valid(Some(CompleteBasisOfClaimAnswer(basisOfClaim)))
            }
          case None        => Valid(None)
        }
    }

  def validateCommodityDetailsAnswer(
    maybeClaimsAnswer: Option[CommoditiesDetailsAnswer]
  ): Validation[CompleteCommodityDetailsAnswer] =
    maybeClaimsAnswer match {
      case Some(value) =>
        value match {
          case CommoditiesDetailsAnswer.IncompleteCommoditiesDetailsAnswer(_)  =>
            invalid("incomplete commodity detail answer")
          case completeCommodityDetailsAnswers: CompleteCommodityDetailsAnswer =>
            Valid(completeCommodityDetailsAnswers)
        }
      case None        => invalid("missing commodity details answer")
    }

  def validateClaimsAnswer(
    maybeClaimsAnswer: Option[ClaimsAnswer]
  ): Validation[CompleteClaimsAnswer] =
    maybeClaimsAnswer match {
      case Some(value) =>
        value match {
          case ClaimsAnswer.IncompleteClaimsAnswer(_)     =>
            invalid("incomplete claims answer")
          case completeClaimsAnswer: CompleteClaimsAnswer =>
            Valid(completeClaimsAnswer)
        }
      case None        => invalid("missing supporting evidence answer")
    }

  def validateSupportingEvidenceAnswer(
    maybeSupportingEvidenceAnswer: Option[SupportingEvidenceAnswer]
  ): Validation[CompleteSupportingEvidenceAnswer] =
    maybeSupportingEvidenceAnswer match {
      case Some(value) =>
        value match {
          case SupportingEvidenceAnswer.IncompleteSupportingEvidenceAnswer(_)     =>
            invalid("incomplete supporting evidence answer")
          case completeSupportingEvidenceAnswer: CompleteSupportingEvidenceAnswer =>
            Valid(completeSupportingEvidenceAnswer)
        }
      case None        => invalid("missing supporting evidence answer")
    }

  def validateBasisOfClaimAnswer(
    maybeBasisOfClaimAnswer: Option[BasisOfClaimAnswer]
  ): Validation[Option[CompleteBasisOfClaimAnswer]] =
    maybeBasisOfClaimAnswer match {
      case Some(value) =>
        value match {
          case BasisOfClaimAnswer.IncompleteBasisOfClaimAnswer(
                _
              ) =>
            invalid("incomplete basis of claim answer")
          case completeBasisOfClaimAnswer: CompleteBasisOfClaimAnswer =>
            Valid(Some(completeBasisOfClaimAnswer))
        }
      case None        => Valid(None)
    }

  def validateBankAccountDetailAnswer(
    maybeBankAccountDetailsAnswer: Option[BankAccountDetailsAnswer]
  ): Validation[Option[CompleteBankAccountDetailAnswer]] =
    maybeBankAccountDetailsAnswer match {
      case Some(value) =>
        value match {
          case BankAccountDetailsAnswer.IncompleteBankAccountDetailAnswer(_)    =>
            invalid("incomplete bank details type answer")
          case completeBankAccountDetailAnswer: CompleteBankAccountDetailAnswer =>
            Valid(Some(completeBankAccountDetailAnswer))
        }
      case None        => Valid(None) //check
    }

  def validateClaimantDetailsAsImporterAnswer(
    maybeClaimantDetailsAsImporterCompanyAnswer: Option[ContactDetailsAnswer]
  ): Validation[Option[CompleteContactDetailsAnswer]] =
    maybeClaimantDetailsAsImporterCompanyAnswer match {
      case Some(value) =>
        value match {
          case ContactDetailsAnswer.IncompleteContactDetailsAnswer(
                _
              ) =>
            invalid("incomplete claimant details as importer answer")
          case completeClaimantDetailsAsImporterCompanyAnswer: CompleteContactDetailsAnswer =>
            Valid(Some(completeClaimantDetailsAsImporterCompanyAnswer))
        }
      case None        => Valid(None)
    }

  def validateClaimantDetailsAsIndividualAnswer(
    maybeClaimantDetailsAsIndividualAnswer: Option[ClaimantDetailsAsIndividualAnswer]
  ): Validation[CompleteClaimantDetailsAsIndividualAnswer] =
    maybeClaimantDetailsAsIndividualAnswer match {
      case Some(value) =>
        value match {
          case ClaimantDetailsAsIndividualAnswer.IncompleteClaimantDetailsAsIndividualAnswer(_) =>
            invalid("incomplete claimant details type answer")
          case c: CompleteClaimantDetailsAsIndividualAnswer                                     => Valid(c)
        }
      case None        => invalid("missing claimant details type answer")
    }

  def validateDeclarantTypeAnswer(
    maybeDeclarantTypeAnswer: Option[DeclarantTypeAnswer]
  ): Validation[CompleteDeclarantTypeAnswer] =
    maybeDeclarantTypeAnswer match {
      case Some(value) =>
        value match {
          case DeclarantTypeAnswer.IncompleteDeclarantTypeAnswer(_) =>
            invalid("incomplete declarant type answer")
          case c: CompleteDeclarantTypeAnswer                       => Valid(c)
        }
      case None        => invalid("missing declarant type answer")
    }

  def validateDeclarationDetailsAnswer(
    maybeDeclarationDetailsAnswer: Option[DeclarationDetailsAnswer]
  ): Validation[CompleteDeclarationDetailsAnswer] =
    maybeDeclarationDetailsAnswer match {
      case Some(value) =>
        value match {
          case DeclarationDetailsAnswer.IncompleteDeclarationDetailsAnswer(_)     =>
            invalid("incomplete declaration details answer")
          case completeDeclarationDetailsAnswer: CompleteDeclarationDetailsAnswer =>
            Valid(completeDeclarationDetailsAnswer)
        }
      case None        => invalid("missing declaration details answer")
    }

  def validateDuplicateMovementReferenceNumberAnswer(
    maybeDuplicateMovementReferenceNumberAnswer: Option[DuplicateMovementReferenceNumberAnswer]
  ): Validation[Option[CompleteDuplicateMovementReferenceNumberAnswer]] =
    maybeDuplicateMovementReferenceNumberAnswer match {
      case Some(value) =>
        value match {
          case DuplicateMovementReferenceNumberAnswer.IncompleteDuplicateMovementReferenceNumberAnswer(
                _
              ) =>
            invalid("incomplete duplicate movement reference number")
          case completeDuplicateMovementReferenceNumberAnswer: CompleteDuplicateMovementReferenceNumberAnswer =>
            Valid(Some(completeDuplicateMovementReferenceNumberAnswer))
        }
      case None        => Valid(None)
    }

  def validateDuplicateDeclarantDetailAnswer(
    maybeDuplicateDeclarantDetailAnswers: Option[DuplicateDeclarationDetailsAnswer]
  ): Validation[CompleteDuplicateDeclarationDetailsAnswer] =
    maybeDuplicateDeclarantDetailAnswers match {
      case Some(value) =>
        value match {
          case DuplicateDeclarationDetailsAnswer.IncompleteDuplicateDeclarationDetailAnswer(_)     =>
            invalid("incomplete duplicate declaration details answer")
          case completeDuplicateDeclarationDetailAnswer: CompleteDuplicateDeclarationDetailsAnswer =>
            Valid(completeDuplicateDeclarationDetailAnswer)
        }
      case None        => Valid(CompleteDuplicateDeclarationDetailsAnswer(None))
    }

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
            maybeDisplayDeclaration,
            _,
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
            maybeDisplayDeclaration,
            _,
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
            _
          ) =>
        duplicateDeclarationDetails match {
          case Some(completeDuplicateDeclarationDetailsAnswer) =>
            completeDuplicateDeclarationDetailsAnswer.duplicateDeclaration
          case None                                            => None
        }
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
            _
          ) =>
        declarationDetailAnswers match {
          case Some(completeDeclarationDetailsAnswer) => Some(completeDeclarationDetailsAnswer.declarationDetails)
          case None                                   => None
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
            _
          ) =>
        declarantType.declarantType
    }

    def basisForClaim: Either[SelectReasonForClaimAndBasis, BasisOfClaim] = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            maybeBasisForClaim,
            _,
            _,
            _,
            maybeReasonForClaimAndBasisAnswer,
            _,
            _,
            _,
            _,
            _
          ) =>
        (maybeReasonForClaimAndBasisAnswer, maybeBasisForClaim) match {
          case (Some(rc), None) => Left(rc.selectReasonForBasisAndClaim)
          case (None, Some(r))  => Right(r.basisOfClaim)
          case (None, None)     =>
            sys.error(
              "invalid state: either select reason for basis and claim or reason for claim should have been provided"
            )
          case _                => sys.error("invalid state: cannot have both reason-for-claim-and-basis and reason-for-claim")
        }
    }

    def claimantDetailsAsImporterCompany: Option[ContactDetailsFormData] = completeClaim match {
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
            _
          ) =>
        claimantDetailsAsImporterCompanyAnswer match {
          case Some(value) => Some(value.contactDetailsFormData)
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

    def bankDetails: Option[BankAccountController.BankAccountDetails] = completeClaim match {
      case CompleteC285Claim(
            _,
            completeMovementReferenceNumberAnswer,
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
            maybeDisplayDeclaration,
            _,
            _,
            _,
            _
          ) =>
        completeMovementReferenceNumberAnswer.movementReferenceNumber match {
          case Left(_)  =>
            bankAccountDetails match {
              case Some(bankAccountDetailAnswer) => Some(bankAccountDetailAnswer.bankAccountDetails)
              case None                          => None
            }
          case Right(_) =>
            bankAccountDetails match {
              case Some(bankAccountDetailAnswer) =>
                Some(bankAccountDetailAnswer.bankAccountDetails)
              case None                          =>
                maybeDisplayDeclaration match {
                  case Some(value) =>
                    value.displayResponseDetail.maskedBankDetails match {
                      case Some(maybeBankDetails) =>
                        maybeBankDetails.consigneeBankDetails match {
                          case Some(value) =>
                            Some(
                              BankAccountController.BankAccountDetails(
                                AccountName(value.accountHolderName),
                                Some(false),
                                SortCode(value.sortCode),
                                AccountNumber(value.accountNumber)
                              )
                            )
                          case None        => None
                        }
                      case None                   => None
                    }
                  case None        => None
                }
            }
        }
    }

    //TODO: fixme
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
            _,
            bankAccountDetails,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        bankAccountDetails match {
          case Some(value) =>
            value.bankAccountDetails.isBusinessAccount match {
              case Some(value) => if (value === true) "Business Account" else "Non-Business Account"
              case None        => ""
            }
          case None        => ""
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
            _
          ) =>
        movementReferenceNumber.movementReferenceNumber
    }

    def duplicateMovementReferenceNumber: Option[Either[EntryNumber, MRN]] = completeClaim match {
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
            _
          ) =>
        duplicateMovementReferenceNumberAnswer match {
          case Some(completeDuplicateMovementReferenceNumberAnswer) =>
            completeDuplicateMovementReferenceNumberAnswer.maybeDuplicateMovementReferenceNumber match {
              case Some(number) => Some(number)
              case None         => None
            }
          case _                                                    => None
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
            _,
            supportingEvidenceAnswers,
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

    def totalUKDutyClaim: String = completeClaim match {
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
            completeClaimsAnswer
          ) =>
        def isUKTax(taxCode: String): Boolean =
          TaxCode.listOfUKTaxCodes.map(t => t.toString).exists(p => p.contains(taxCode))
        MoneyUtils.formatAmountOfMoneyWithPoundSign(
          completeClaimsAnswer.claims.filter(p => isUKTax(p.taxCode)).map(s => s.claimAmount).sum
        )
    }

    def totalEuDutyClaim: String = completeClaim match {
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
            completeClaimsAnswer
          ) =>
        def isUKTax(taxCode: String): Boolean =
          TaxCode.listOfEUTaxCodes.map(t => t.toString).exists(p => p.contains(taxCode))
        MoneyUtils.formatAmountOfMoneyWithPoundSign(
          completeClaimsAnswer.claims.filter(p => isUKTax(p.taxCode)).map(s => s.claimAmount).sum
        )
    }

    def totalClaim: String = completeClaim match {
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
            completeClaimsAnswer
          ) =>
        MoneyUtils.formatAmountOfMoneyWithPoundSign(completeClaimsAnswer.claims.map(c => c.claimAmount).sum)
    }

  }

  implicit val eq: Eq[CompleteClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[CompleteClaim] = derived.oformat()

}
