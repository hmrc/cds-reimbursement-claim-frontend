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
import cats.data.Validated
import cats.data.Validated.{Valid, invalidNel}
import cats.syntax.all._
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.{consigneeToClaimantDetails, declarantToClaimantDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SelectNumberOfClaimsAnswer.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, ScheduledDocumentAnswer, SupportingEvidencesAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{DeclarantEoriNumber, ImporterEoriNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MoneyUtils

import java.util.UUID
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementMethodAnswer

final case class CompleteClaim(
  id: UUID,
  movementReferenceNumber: MRN,
  maybeDuplicateMovementReferenceNumberAnswer: Option[MRN],
  declarantTypeAnswer: DeclarantTypeAnswer,
  detailsRegisteredWithCdsAnswer: DetailsRegisteredWithCdsAnswer,
  mrnContactDetailsAnswer: Option[MrnContactDetails],
  mrnContactAddressAnswer: Option[ContactAddress],
  maybeBasisOfClaimAnswer: Option[BasisOfClaim],
  maybeBankAccountDetailsAnswer: Option[BankAccountDetails],
  supportingEvidencesAnswer: SupportingEvidencesAnswer,
  commodityDetailsAnswer: CommodityDetails,
  northernIrelandAnswer: Option[ClaimNorthernIrelandAnswer],
  maybeDisplayDeclaration: Option[DisplayDeclaration],
  maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration],
  importerEoriNumber: Option[ImporterEoriNumber],
  declarantEoriNumber: Option[DeclarantEoriNumber],
  claimsAnswer: ClaimsAnswer,
  scheduledDocumentAnswer: Option[ScheduledDocumentAnswer],
  reimbursementMethodAnswer: Option[ReimbursementMethodAnswer]
)

object CompleteClaim {

  def fromDraftClaim(draftClaim: DraftClaim, verifiedEmail: Email): Either[Error, CompleteClaim] =
    draftClaim match {
      case DraftClaim(
            id,
            typeOfClaim,
            Some(mrn),
            maybeDuplicateMovementReferenceNumber,
            draftDeclarantTypeAnswer,
            _,
            draftMrnContactDetails,
            draftMrnContactAddress,
            maybeBankAccountDetails,
            _,
            maybeBasisForClaim,
            maybeSupportingEvidences,
            _,
            _,
            _,
            _,
            draftCommodityAnswer,
            draftNorthernIrelandAnswer,
            maybeDisplayDeclaration,
            maybeDuplicateDisplayDeclaration,
            draftImporterEoriNumberAnswer,
            draftDeclarantEoriNumberAnswer,
            Some(claimsAnswer),
            maybeScheduledDocument,
            _,
            _,
            maybeReimbursementMethodAnswer
          ) =>
        (
          validateDeclarantTypeAnswer(draftDeclarantTypeAnswer),
          validateDetailsRegisteredWithCdsMrn(draftDeclarantTypeAnswer, maybeDisplayDeclaration, verifiedEmail),
          validateSupportingEvidencesAnswer(maybeSupportingEvidences),
          validateCommodityDetailsAnswer(draftCommodityAnswer),
          validateImporterEoriNumberAnswer(draftImporterEoriNumberAnswer),
          validateDeclarantEoriNumberAnswer(draftDeclarantEoriNumberAnswer),
          validateScheduledDocumentAnswer(maybeScheduledDocument, typeOfClaim)
        )
          .mapN {
            case (
                  declarantTypeAnswer,
                  detailsRegisteredWithCdsAnswer,
                  supportingEvidenceAnswer,
                  commodityDetailsAnswer,
                  importerEoriNumberAnswer,
                  declarantEoriNumberAnswer,
                  maybeScheduledDocumentAnswer
                ) =>
              CompleteClaim(
                id = id,
                movementReferenceNumber = mrn,
                maybeDuplicateMovementReferenceNumberAnswer = maybeDuplicateMovementReferenceNumber,
                declarantTypeAnswer,
                detailsRegisteredWithCdsAnswer,
                draftMrnContactDetails,
                draftMrnContactAddress,
                maybeBasisForClaim,
                maybeBankAccountDetailsAnswer = maybeBankAccountDetails,
                supportingEvidenceAnswer,
                commodityDetailsAnswer,
                draftNorthernIrelandAnswer,
                maybeDisplayDeclaration,
                maybeDuplicateDisplayDeclaration,
                importerEoriNumberAnswer,
                declarantEoriNumberAnswer,
                claimsAnswer,
                maybeScheduledDocumentAnswer,
                maybeReimbursementMethodAnswer
              )
          }
          .toEither
          .leftMap { errors =>
            Error(
              s"could not create complete claim in order to submit claim request: ${errors.toList.mkString("; ")}"
            )
          }

      case _ => Left(Error("unknown claim type"))
    }

  implicit val eq: Eq[CompleteClaim]          = Eq.fromUniversalEquals[CompleteClaim]
  implicit val format: OFormat[CompleteClaim] = Json.format[CompleteClaim]

  def validateDeclarantTypeAnswer(
    maybeDeclarantTypeAnswer: Option[DeclarantTypeAnswer]
  ): Validation[DeclarantTypeAnswer] =
    maybeDeclarantTypeAnswer toValidNel "missing declarant type answer"

  def validateClaimsAnswer(maybeClaimsAnswer: Option[ClaimsAnswer]): Validation[ClaimsAnswer] =
    maybeClaimsAnswer toValidNel "missing claims answer"

  def validateDetailsRegisteredWithCdsEntryNumber(
    maybeDetailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer]
  ): Validation[DetailsRegisteredWithCdsAnswer] =
    maybeDetailsRegisteredWithCdsAnswer toValidNel "missing claimant details type answer"

  def validateCommodityDetailsAnswer(
    maybeClaimsAnswer: Option[CommodityDetails]
  ): Validation[CommodityDetails] =
    maybeClaimsAnswer toValidNel "missing commodity details answer"

  def validateSupportingEvidencesAnswer(
    maybeSupportingEvidencesAnswer: Option[SupportingEvidencesAnswer]
  ): Validation[SupportingEvidencesAnswer] =
    maybeSupportingEvidencesAnswer toValidNel "missing supporting evidences answer"

  def validateClaimantContactDetailsEntryNumber(
    maybeMrnContactDetails: Option[MrnContactDetails]
  ): Validation[Option[MrnContactDetails]] =
    maybeMrnContactDetails match {
      case None => invalidNel("incomplete contact details")
      case a    => Valid(a)
    }

  def validateDeclarantEoriNumberAnswer(
    maybeDeclarantEoriNumberAnswer: Option[DeclarantEoriNumber]
  ): Validation[Option[DeclarantEoriNumber]] =
    maybeDeclarantEoriNumberAnswer match {
      case Some(value) =>
        value match {
          case DeclarantEoriNumber(_) => Valid(Some(value))
          case _                      => invalidNel("incomplete declarant eori number answer")
        }
      case None        => Valid(None)
    }

  def validateImporterEoriNumberAnswer(
    maybeImporterEoriNumberAnswer: Option[ImporterEoriNumber]
  ): Validation[Option[ImporterEoriNumber]] =
    maybeImporterEoriNumberAnswer match {
      case Some(value) =>
        value match {
          case ImporterEoriNumber(_) => Valid(Some(value))
          case _                     => invalidNel("incomplete importer eori number answer")
        }
      case None        => Valid(None)
    }

  def validateDetailsRegisteredWithCdsMrn(
    maybeDeclarantType: Option[DeclarantTypeAnswer],
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    verifiedEmail: Email
  ): Validation[DetailsRegisteredWithCdsAnswer] =
    (maybeDeclarantType, maybeDisplayDeclaration)
      .mapN { (declarantType, displayDeclaration) =>
        val detailsRegisteredWithCdsFormData = declarantType match {
          case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
            consigneeToClaimantDetails(displayDeclaration, verifiedEmail)
          case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
            declarantToClaimantDetails(displayDeclaration, verifiedEmail)
        }
        Valid(detailsRegisteredWithCdsFormData)
      }
      .getOrElse(invalidNel("Missing declarant type or display declaration"))

  def validateScheduledDocumentAnswer(
    maybeScheduledDocument: Option[ScheduledDocumentAnswer],
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): Validation[Option[ScheduledDocumentAnswer]] =
    Validated.condNel(
      numberOfClaims.forall(answer =>
        (answer === Scheduled && maybeScheduledDocument.isDefined) ||
          (answer =!= Scheduled && maybeScheduledDocument.isEmpty)
      ),
      maybeScheduledDocument,
      "Scheduled document is either missing for Scheduled journey or was present in other type of journeys"
    )

  implicit class CompleteClaimOps(private val completeClaim: CompleteClaim) extends AnyVal {

    def bankDetails: Option[BankAccountDetails] =
      completeClaim.maybeBankAccountDetailsAnswer match {
        case None =>
          for {
            displayDeclaration   <- completeClaim.maybeDisplayDeclaration
            bankDetails          <- displayDeclaration.displayResponseDetail.maskedBankDetails
            consigneeBankDetails <- bankDetails.consigneeBankDetails
          } yield BankAccountDetails(
            AccountName(consigneeBankDetails.accountHolderName),
            SortCode(consigneeBankDetails.sortCode),
            AccountNumber(consigneeBankDetails.accountNumber)
          )
        case _    => completeClaim.maybeBankAccountDetailsAnswer
      }

    def bankAccountType: String = BankAccountType.allAccountTypes.map(_.value).toString()

    def totalUKDutyClaim: String = {
      def isUKTax(taxCode: String): Boolean =
        TaxCode.listOfUKTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

      MoneyUtils.formatAmountOfMoneyWithPoundSign(
        completeClaim.claimsAnswer.filter(p => isUKTax(p.taxCode)).map(s => s.claimAmount).sum
      )
    }

    def totalEuDutyClaim: String = {
      def isEuTax(taxCode: String): Boolean =
        TaxCode.listOfEUTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

      MoneyUtils.formatAmountOfMoneyWithPoundSign(
        completeClaim.claimsAnswer.filter(p => isEuTax(p.taxCode)).map(s => s.claimAmount).sum
      )
    }

    def totalExciseDutyClaim: String = {
      def isExciseTax(taxCode: String): Boolean =
        TaxCode.listOfUKExciseCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

      MoneyUtils.formatAmountOfMoneyWithPoundSign(
        completeClaim.claimsAnswer.filter(p => isExciseTax(p.taxCode)).map(s => s.claimAmount).sum
      )
    }

    def totalClaim: String = {
      val sum = completeClaim.claimsAnswer.toList.map(_.claimAmount).sum
      MoneyUtils.formatAmountOfMoneyWithPoundSign(sum)
    }
  }
}
