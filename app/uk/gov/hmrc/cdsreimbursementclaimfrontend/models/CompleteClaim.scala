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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, ScheduledDocumentAnswer, SupportingEvidencesAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument

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
    maybeBankAccountDetailsAnswer: Option[BankAccountDetails],
    supportingEvidencesAnswer: SupportingEvidencesAnswer,
    commodityDetailsAnswer: CommodityDetails,
    completeNorthernIrelandAnswer: Option[CompleteNorthernIrelandAnswer],
    maybeCompleteReasonAndBasisOfClaimAnswer: Option[CompleteReasonAndBasisOfClaimAnswer],
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration],
    importerEoriNumber: Option[CompleteImporterEoriNumberAnswer],
    declarantEoriNumber: Option[CompleteDeclarantEoriNumberAnswer],
    claimsAnswer: ClaimsAnswer,
    scheduledDocumentAnswer: Option[ScheduledDocumentAnswer]
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
              _,
              draftClaimantDetailsAsIndividualAnswer,
              draftClaimantDetailsAsImporterCompanyAnswer,
              _,
              maybeBankAccountDetailsAnswer,
              maybeBasisForClaim,
              maybeSupportingEvidences,
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
              _,
              maybeScheduledDocument
            ) =>
          movementReferenceNumber.value match {
            case Left(_) =>
              (
                validateDeclarationDetailsAnswer(draftMaybeDeclarationDetailsAnswer),
                validateDuplicateDeclarantDetailAnswer(draftDuplicateDeclarationDetailAnswer),
                validateDeclarantTypeAnswer(draftDeclarantTypeAnswer),
                validateDetailsRegisteredWithCdsAnswer(draftClaimantDetailsAsIndividualAnswer),
                validateClaimantDetailsAsImporterAnswer(draftClaimantDetailsAsImporterCompanyAnswer),
                validateBankAccountDetailAnswer(maybeBankAccountDetailsAnswer),
                validateSupportingEvidencesAnswer(maybeSupportingEvidences),
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
                        completeBankAccountDetailsAnswer,
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
                      completeBankAccountDetailsAnswer,
                      supportingEvidenceAnswer,
                      completeCommodityDetailsAnswer,
                      completeNorthernIrelandAnswer,
                      completeReasonAndBasisOfClaimAnswer,
                      maybeDisplayDeclaration = None,
                      maybeDuplicateDisplayDeclaration = None,
                      None,
                      None,
                      claimsAnswer,
                      maybeScheduledDocument
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
                validateBankAccountDetailAnswer(maybeBankAccountDetailsAnswer),
                validateSupportingEvidencesAnswer(maybeSupportingEvidences),
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
                        completeBankAccountDetailsAnswer,
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
                      completeBankAccountDetailsAnswer,
                      supportingEvidenceAnswer,
                      completeCommodityDetailsAnswer,
                      completeNorthernIrelandAnswer,
                      None,
                      maybeDisplayDeclaration,
                      maybeDuplicateDisplayDeclaration,
                      completeImporterEoriNumberAnswer,
                      completeDeclarantEoriNumberAnswer,
                      claimsAnswer,
                      maybeScheduledDocument
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

  def validateSupportingEvidencesAnswer(
    maybeSupportingEvidencesAnswer: Option[SupportingEvidencesAnswer]
  ): Validation[SupportingEvidencesAnswer] =
    maybeSupportingEvidencesAnswer toValidNel "missing supporting evidences answer"

  def validateBankAccountDetailAnswer(
    maybeBankAccountDetailsAnswer: Option[BankAccountDetails]
  ): Validation[Option[BankAccountDetails]] =
    maybeBankAccountDetailsAnswer match {
      case value => Valid(value)
      case None  => invalid("incomplete bank details type answer")
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

    def maybeDisplayDeclaration: Option[DisplayDeclaration] =
      completeClaim.get(_.maybeDisplayDeclaration)

    def maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration] =
      completeClaim.get(_.maybeDuplicateDisplayDeclaration)

    def duplicateEntryDeclarationDetails: Option[EntryDeclarationDetails] =
      completeClaim
        .get(_.maybeCompleteDuplicateDeclarationDetailsAnswer)
        .flatMap(_.duplicateDeclaration)

    def entryDeclarationDetails: Option[EntryDeclarationDetails] =
      completeClaim
        .get(_.maybeCompleteDeclarationDetailsAnswer)
        .map(_.declarationDetails)

    def declarantType: DeclarantTypeAnswer =
      completeClaim.get(_.declarantTypeAnswer)

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

    def claimantDetailsAsImporterCompany: Option[ContactDetailsFormData] =
      completeClaim.get(_.maybeContactDetailsAnswer).map(_.contactDetailsFormData)

    def detailsRegisteredWithCds: DetailsRegisteredWithCdsFormData =
      completeClaim.get(_.completeDetailsRegisteredWithCdsAnswer.detailsRegisteredWithCds)

    def commodityDetails: String =
      completeClaim.get(_.commodityDetailsAnswer.value)

    def northernIrelandAnswer: Option[CompleteNorthernIrelandAnswer] =
      completeClaim.get(_.completeNorthernIrelandAnswer)

    def bankDetails: Option[BankAccountDetails] = completeClaim match {
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
            _,
            _
          ) =>
        completeMovementReferenceNumberAnswer.value match {
          case Left(_)  =>
            bankAccountDetails match {
              case Some(bankAccountDetails) => Some(bankAccountDetails)
              case None                     => None
            }
          case Right(_) =>
            bankAccountDetails match {
              case Some(bankAccountDetails) =>
                Some(bankAccountDetails)
              case None                     =>
                maybeDisplayDeclaration match {
                  case Some(value) =>
                    value.displayResponseDetail.maskedBankDetails match {
                      case Some(maybeBankDetails) =>
                        maybeBankDetails.consigneeBankDetails match {
                          case Some(value) =>
                            Some(
                              BankAccountDetails(
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

    def bankAccountType: String =
      completeClaim
        .get(_.bankDetails)
        .flatMap(_.isBusinessAccount)
        .map(isBusiness => if (isBusiness) "Business Account" else "Non-Business Account")
        .getOrElse("")

    def movementReferenceNumber: Either[EntryNumber, MRN] =
      completeClaim.get(_.movementReferenceNumber.value)

    def supportingEvidences: List[UploadDocument] =
      completeClaim.get(_.supportingEvidencesAnswer.toList)

    def totalUKDutyClaim: String = {
      def isUKTax(taxCode: String): Boolean =
        TaxCode.listOfUKTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

      val claimsAnswer = completeClaim.get(_.claimsAnswer)

      MoneyUtils.formatAmountOfMoneyWithPoundSign(
        claimsAnswer.filter(p => isUKTax(p.taxCode)).map(s => s.claimAmount).sum
      )
    }

    def totalEuDutyClaim: String = {
      def isEuTax(taxCode: String): Boolean =
        TaxCode.listOfEUTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

      val claimsAnswer = completeClaim.get(_.claimsAnswer)

      MoneyUtils.formatAmountOfMoneyWithPoundSign(
        claimsAnswer.filter(p => isEuTax(p.taxCode)).map(s => s.claimAmount).sum
      )
    }

    def totalExciseDutyClaim: String = {
      def isExciseTax(taxCode: String): Boolean =
        TaxCode.listOfUKExciseCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

      val claimsAnswer = completeClaim.get(_.claimsAnswer)

      MoneyUtils.formatAmountOfMoneyWithPoundSign(
        claimsAnswer.filter(p => isExciseTax(p.taxCode)).map(s => s.claimAmount).sum
      )
    }

    def totalClaim: String = {
      val claimsAnswer = completeClaim.get(_.claimsAnswer)
      MoneyUtils.formatAmountOfMoneyWithPoundSign(claimsAnswer.toList.map(_.claimAmount).sum)
    }

    def get[A](f: CompleteC285Claim => A): A =
      f(completeClaim.asInstanceOf[CompleteC285Claim])
  }

  implicit val eq: Eq[CompleteClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[CompleteClaim] = derived.oformat()

}
