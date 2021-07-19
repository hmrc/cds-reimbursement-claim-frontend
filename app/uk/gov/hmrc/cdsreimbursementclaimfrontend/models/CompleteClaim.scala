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
import cats.syntax.all._
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.{AccountName, AccountNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.DetailsRegisteredWithCdsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterYourContactDetailsController.ContactDetailsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectReasonForBasisAndClaimController.SelectReasonForClaimAndBasis
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.CompleteBankAccountDetailAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsAnswer.CompleteContactDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantEoriNumberAnswer.CompleteDeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.CompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.CompleteDetailsRegisteredWithCdsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateDeclarationDetailsAnswer.CompleteDuplicateDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ImporterEoriNumberAnswer.CompleteImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonAndBasisOfClaimAnswer.CompleteReasonAndBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, SupportingEvidenceAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence

import java.util.UUID

sealed trait CompleteClaim extends Product with Serializable {
  val id: UUID
}

object CompleteClaim {

  final case class CompleteC285Claim(
    id: UUID,
    movementReferenceNumber: MovementReferenceNumber,
    maybeDuplicateMovementReferenceNumberAnswer: Option[MovementReferenceNumber],
    maybeCompleteDeclarationDetailsAnswer: Option[CompleteDeclarationDetailsAnswer],
    maybeCompleteDuplicateDeclarationDetailsAnswer: Option[CompleteDuplicateDeclarationDetailsAnswer],
    declarantTypeAnswer: DeclarantTypeAnswer,
    completeDetailsRegisteredWithCdsAnswer: CompleteDetailsRegisteredWithCdsAnswer,
    maybeContactDetailsAnswer: Option[CompleteContactDetailsAnswer],
    maybeBasisOfClaimAnswer: Option[BasisOfClaim],
    maybeCompleteBankAccountDetailAnswer: Option[CompleteBankAccountDetailAnswer],
    supportingEvidenceAnswer: SupportingEvidenceAnswer,
    commodityDetailsAnswer: CommodityDetails,
    completeNorthernIrelandAnswer: Option[CompleteNorthernIrelandAnswer],
    maybeCompleteReasonAndBasisOfClaimAnswer: Option[CompleteReasonAndBasisOfClaimAnswer],
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration],
    importerEoriNumber: Option[CompleteImporterEoriNumberAnswer],
    declarantEoriNumber: Option[CompleteDeclarantEoriNumberAnswer],
    claimsAnswer: ClaimsAnswer
  ) extends CompleteClaim

  object CompleteC285Claim {

    def fromDraftClaim(draftClaim: DraftClaim): Either[Error, CompleteC285Claim] =
      draftClaim match {
        case DraftClaim.DraftC285Claim(
              id,
              _,
              Some(movementReferenceNumber: MovementReferenceNumber),
              maybeDuplicateMovementReferenceNumber,
              draftMaybeDeclarationDetailsAnswer,
              draftDuplicateDeclarationDetailAnswer,
              draftDeclarantTypeAnswer,
              draftClaimantDetailsAsIndividualAnswer,
              draftClaimantDetailsAsImporterCompanyAnswer,
              draftBankAccountDetailAnswer,
              maybeBasisForClaim,
              draftSupportingEvidences,
              _,
              draftCommodityAnswer,
              draftNorthernIrelandAnswer,
              draftReasonAndBasisOfClaimAnswer,
              maybeDisplayDeclaration,
              maybeDuplicateDisplayDeclaration,
              draftImporterEoriNumberAnswer,
              draftDeclarantEoriNumberAnswer,
              Some(claimsAnswer),
              _,
              _
            ) =>
          movementReferenceNumber.value match {
            case Left(_) =>
              (
                validateDeclarationDetailsAnswer(draftMaybeDeclarationDetailsAnswer),
                validateDuplicateDeclarantDetailAnswer(draftDuplicateDeclarationDetailAnswer),
                validateDeclarantTypeAnswer(draftDeclarantTypeAnswer),
                validateDetailsRegisteredWithCdsAnswer(draftClaimantDetailsAsIndividualAnswer),
                validateClaimantDetailsAsImporterAnswer(draftClaimantDetailsAsImporterCompanyAnswer),
                validateBankAccountDetailAnswer(draftBankAccountDetailAnswer),
                validateSupportingEvidenceAnswer(draftSupportingEvidences),
                validateCommodityDetailsAnswer(draftCommodityAnswer),
                validateNorthernIrelandAnswer(draftNorthernIrelandAnswer),
                validateReasonAndBasisOfClaimAnswer(draftReasonAndBasisOfClaimAnswer)
              )
                .mapN {
                  case (
                        completeDeclarationDetailsAnswer,
                        completeDuplicateDeclarationDetailAnswer,
                        completeDeclarantTypeAnswer,
                        completeClaimantDetailsAsIndividualAnswer,
                        completeClaimantDetailsAsImporterCompanyAnswer,
                        completeBankAccountDetailAnswer,
                        supportingEvidenceAnswer,
                        completeCommodityDetailsAnswer,
                        completeNorthernIrelandAnswer,
                        completeReasonAndBasisOfClaimAnswer
                      ) =>
                    CompleteC285Claim(
                      id = id,
                      movementReferenceNumber = movementReferenceNumber,
                      maybeDuplicateMovementReferenceNumberAnswer = maybeDuplicateMovementReferenceNumber,
                      maybeCompleteDeclarationDetailsAnswer = Some(completeDeclarationDetailsAnswer),
                      maybeCompleteDuplicateDeclarationDetailsAnswer = Some(completeDuplicateDeclarationDetailAnswer),
                      completeDeclarantTypeAnswer,
                      completeClaimantDetailsAsIndividualAnswer,
                      completeClaimantDetailsAsImporterCompanyAnswer,
                      maybeBasisForClaim,
                      completeBankAccountDetailAnswer,
                      supportingEvidenceAnswer,
                      completeCommodityDetailsAnswer,
                      completeNorthernIrelandAnswer,
                      completeReasonAndBasisOfClaimAnswer,
                      maybeDisplayDeclaration = None,
                      maybeDuplicateDisplayDeclaration = None,
                      None,
                      None,
                      claimsAnswer
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
                validateDeclarantTypeAnswer(draftDeclarantTypeAnswer),
                validateDetailsRegisteredWithCdsAnswer(draftClaimantDetailsAsIndividualAnswer),
                validateClaimantDetailsAsImporterAnswer(draftClaimantDetailsAsImporterCompanyAnswer),
                validateBankAccountDetailAnswer(draftBankAccountDetailAnswer),
                validateSupportingEvidenceAnswer(draftSupportingEvidences),
                validateCommodityDetailsAnswer(draftCommodityAnswer),
                validateNorthernIrelandAnswer(draftNorthernIrelandAnswer),
                validateImporterEoriNumberAnswer(draftImporterEoriNumberAnswer),
                validateDeclarantEoriNumberAnswer(draftDeclarantEoriNumberAnswer)
              )
                .mapN {
                  case (
                        completeDeclarantTypeAnswer,
                        completeClaimantDetailsAsIndividualAnswer,
                        completeClaimantDetailsAsImporterCompanyAnswer,
                        completeBankAccountDetailAnswer,
                        supportingEvidenceAnswer,
                        completeCommodityDetailsAnswer,
                        completeNorthernIrelandAnswer,
                        completeImporterEoriNumberAnswer,
                        completeDeclarantEoriNumberAnswer
                      ) =>
                    CompleteC285Claim(
                      id = id,
                      movementReferenceNumber = movementReferenceNumber,
                      maybeDuplicateMovementReferenceNumberAnswer = maybeDuplicateMovementReferenceNumber,
                      maybeCompleteDeclarationDetailsAnswer = None,
                      maybeCompleteDuplicateDeclarationDetailsAnswer = None,
                      completeDeclarantTypeAnswer,
                      completeClaimantDetailsAsIndividualAnswer,
                      completeClaimantDetailsAsImporterCompanyAnswer,
                      maybeBasisForClaim,
                      completeBankAccountDetailAnswer,
                      supportingEvidenceAnswer,
                      completeCommodityDetailsAnswer,
                      completeNorthernIrelandAnswer,
                      None,
                      maybeDisplayDeclaration,
                      maybeDuplicateDisplayDeclaration,
                      completeImporterEoriNumberAnswer,
                      completeDeclarantEoriNumberAnswer,
                      claimsAnswer
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

  def validateCommodityDetailsAnswer(
    maybeClaimsAnswer: Option[CommodityDetails]
  ): Validation[CommodityDetails] =
    maybeClaimsAnswer toValidNel "Missing commodity details answer"

  def validateNorthernIrelandAnswer(
    maybeNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer]
  ): Validation[Option[CompleteNorthernIrelandAnswer]] =
    maybeNorthernIrelandAnswer match {
      case Some(claimNorthernIrelandAnswer) => Valid(Some(CompleteNorthernIrelandAnswer(claimNorthernIrelandAnswer)))
      case None                             => Valid(None)
    }

  def validateClaimsAnswer(maybeClaimsAnswer: Option[ClaimsAnswer]): Validation[ClaimsAnswer] =
    maybeClaimsAnswer match {
      case Some(value) => Valid(value)
      case None        => invalid("missing supporting evidence answer")
    }

  def validateSupportingEvidenceAnswer(
    maybeSupportingEvidenceAnswer: Option[SupportingEvidenceAnswer]
  ): Validation[SupportingEvidenceAnswer] =
    maybeSupportingEvidenceAnswer toValidNel "missing supporting evidence answer"

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

  def validateDetailsRegisteredWithCdsAnswer(
    maybeDetailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer]
  ): Validation[CompleteDetailsRegisteredWithCdsAnswer] =
    maybeDetailsRegisteredWithCdsAnswer match {
      case Some(value) =>
        value match {
          case DetailsRegisteredWithCdsAnswer.IncompleteDetailsRegisteredWithCdsAnswer(_) =>
            invalid("incomplete claimant details type answer")
          case c: CompleteDetailsRegisteredWithCdsAnswer                                  => Valid(c)
        }
      case None        => invalid("missing claimant details type answer")
    }

  def validateDeclarantTypeAnswer(
    maybeDeclarantTypeAnswer: Option[DeclarantTypeAnswer]
  ): Validation[DeclarantTypeAnswer] =
    maybeDeclarantTypeAnswer match {
      case Some(value) => Valid(value)
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
            _,
            _
          ) =>
        declarationDetailAnswers match {
          case Some(completeDeclarationDetailsAnswer) => Some(completeDeclarationDetailsAnswer.declarationDetails)
          case None                                   => None
        }
    }

    def declarantType: DeclarantTypeAnswer = completeClaim match {
      case cc: CompleteC285Claim => cc.declarantTypeAnswer
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
          case (None, Some(r))  => Right(r)
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
            _,
            _
          ) =>
        claimantDetailsAsImporterCompanyAnswer match {
          case Some(value) => Some(value.contactDetailsFormData)
          case None        => None
        }
    }

    def detailsRegisteredWithCds: DetailsRegisteredWithCdsFormData = completeClaim match {
      case CompleteC285Claim(
            _,
            _,
            _,
            _,
            _,
            _,
            detailsRegisteredWithCdsAnswer,
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
        detailsRegisteredWithCdsAnswer.detailsRegisteredWithCds
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
            _,
            _
          ) =>
        commodityDetails.value
    }

    def northernIrelandAnswer: Option[CompleteNorthernIrelandAnswer] = completeClaim match {
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
            northernIrelandAnswer,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        northernIrelandAnswer
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
            _,
            maybeDisplayDeclaration,
            _,
            _,
            _,
            _
          ) =>
        completeMovementReferenceNumberAnswer.value match {
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
      case claim: CompleteC285Claim =>
        claim.movementReferenceNumber.value
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
            supportingEvidences,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        supportingEvidences.toList
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
            _,
            claimsAnswer
          ) =>
        def isUKTax(taxCode: String): Boolean =
          TaxCode.listOfUKTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))
        MoneyUtils.formatAmountOfMoneyWithPoundSign(
          claimsAnswer.filter(p => isUKTax(p.taxCode)).map(s => s.claimAmount).sum
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
            _,
            claimsAnswer
          ) =>
        def isEuTax(taxCode: String): Boolean =
          TaxCode.listOfEUTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))
        MoneyUtils.formatAmountOfMoneyWithPoundSign(
          claimsAnswer.filter(p => isEuTax(p.taxCode)).map(s => s.claimAmount).sum
        )
    }

    def totalExciseDutyClaim: String = completeClaim match {
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
            _,
            claimsAnswer
          ) =>
        def isExciseTax(taxCode: String): Boolean =
          TaxCode.listOfUKExciseCodes.map(t => t.toString()).exists(p => p.contains(taxCode))
        MoneyUtils.formatAmountOfMoneyWithPoundSign(
          claimsAnswer.filter(p => isExciseTax(p.taxCode)).map(s => s.claimAmount).sum
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
            _,
            claimsAnswer
          ) =>
        MoneyUtils.formatAmountOfMoneyWithPoundSign(claimsAnswer.toList.map(_.claimAmount).sum)
    }

  }

  implicit val eq: Eq[CompleteClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[CompleteClaim] = derived.oformat()

}
