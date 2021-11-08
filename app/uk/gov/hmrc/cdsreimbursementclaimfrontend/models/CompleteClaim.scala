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
import cats.data.NonEmptyList
import cats.data.Validated.Valid
import cats.syntax.all._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.{consigneeToClaimantDetails, declarantToClaimantDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer.{Individual, Multiple, Scheduled}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{DeclarantEoriNumberAnswer, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import java.util.UUID

final case class CompleteClaim(
  id: UUID,
  movementReferenceNumber: MRN,
  duplicateMovementReferenceNumberAnswer: Option[MRN],
  declarantTypeAnswer: DeclarantTypeAnswer,
  detailsRegisteredWithCdsAnswer: DetailsRegisteredWithCdsAnswer,
  mrnContactDetailsAnswer: Option[MrnContactDetails],
  mrnContactAddressAnswer: Option[ContactAddress],
  basisOfClaimAnswer: Option[BasisOfClaim],
  bankAccountDetailsAnswer: Option[BankAccountDetails],
  supportingEvidencesAnswer: SupportingEvidencesAnswer,
  commodityDetailsAnswer: CommodityDetailsAnswer,
  northernIrelandAnswer: Option[ClaimNorthernIrelandAnswer],
  displayDeclaration: Option[DisplayDeclaration],
  duplicateDisplayDeclaration: Option[DisplayDeclaration],
  importerEoriNumber: Option[ImporterEoriNumberAnswer],
  declarantEoriNumber: Option[DeclarantEoriNumberAnswer],
  claimedReimbursementsAnswer: ClaimedReimbursementsAnswer,
  reimbursementMethodAnswer: Option[ReimbursementMethodAnswer],
  scheduledDocumentAnswer: Option[ScheduledDocumentAnswer],
  associatedMRNsAnswer: Option[AssociatedMRNsAnswer],
  typeOfClaim: TypeOfClaimAnswer,
  maybeAssociatedMRNsClaimsAnswer: Option[AssociatedMRNsClaimsAnswer]
) {

  lazy val multipleClaimsAnswer: NonEmptyList[(MRN, ClaimedReimbursementsAnswer)] = {
    val mrns   = associatedMRNsAnswer
      .map(mrns => movementReferenceNumber :: mrns)
      .getOrElse(NonEmptyList(movementReferenceNumber, Nil))
    val claims = maybeAssociatedMRNsClaimsAnswer
      .map(claimsAnswers => claimedReimbursementsAnswer :: claimsAnswers)
      .getOrElse(NonEmptyList(claimedReimbursementsAnswer, Nil))
    mrns.zipWith(claims)((m, c) => (m, c))
  }

  lazy val totalReimbursementAmount: BigDecimal =
    typeOfClaim match {
      case Individual => claimedReimbursementsAnswer.total
      case Scheduled  => claimedReimbursementsAnswer.total
      case Multiple   => multipleClaimsAnswer.toList.flatMap(_._2.toList.map(_.claimAmount)).sum
    }
}

object CompleteClaim {

  def detailsRegisteredWithCds(
    declarant: DeclarantTypeAnswer,
    declaration: DisplayDeclaration,
    email: Email
  ): DetailsRegisteredWithCdsAnswer =
    declarant match {
      case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
        consigneeToClaimantDetails(declaration, email)
      case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
        declarantToClaimantDetails(declaration, email)
    }

  def fromDraftClaim(draftClaim: DraftClaim, verifiedEmail: Email): Either[Error, CompleteClaim] =
    draftClaim match {
      case DraftClaim(
            id,
            maybeTypeOfClaim,
            maybeMrn,
            maybeDuplicateMovementReferenceNumberAnswer,
            maybeDraftDeclarantTypeAnswer,
            _,
            maybeDraftMrnContactDetails,
            maybeDraftMrnContactAddress,
            maybeBankAccountDetails,
            _,
            maybeBasisForClaim,
            maybeSupportingEvidences,
            _,
            maybeDraftCommodityAnswer,
            maybeDraftNorthernIrelandAnswer,
            maybeDisplayDeclaration,
            maybeDuplicateDisplayDeclaration,
            maybeImporterEoriNumberAnswer,
            maybeDeclarantEoriNumberAnswer,
            Some(claimedReimbursementsAnswer),
            maybeReimbursementMethodAnswer,
            maybeScheduledDocument,
            maybeAssociatedMRNs,
            _,
            _,
            maybeAssociatedMRNsClaimsAnswer,
            _
          ) =>
        (
          MRN.validator.validate(maybeMrn),
          DisplayDeclaration.validator.validate(maybeDisplayDeclaration),
          DeclarantTypeAnswer.validator.validate(maybeDraftDeclarantTypeAnswer),
          SupportingEvidencesAnswer.validator.validate(maybeSupportingEvidences),
          CommodityDetailsAnswer.validator.validate(maybeDraftCommodityAnswer),
          if (maybeTypeOfClaim.exists(_ === Scheduled))
            ScheduledDocumentAnswer.validator.validate(maybeScheduledDocument)
          else Valid(None)
        ).mapN { case (mrn, declaration, declarant, evidences, commodity, schedule) =>
          CompleteClaim(
            id,
            mrn,
            maybeDuplicateMovementReferenceNumberAnswer,
            declarant,
            detailsRegisteredWithCds(declarant, declaration, verifiedEmail),
            maybeDraftMrnContactDetails,
            maybeDraftMrnContactAddress,
            maybeBasisForClaim,
            maybeBankAccountDetails,
            evidences,
            commodity,
            maybeDraftNorthernIrelandAnswer,
            maybeDisplayDeclaration,
            maybeDuplicateDisplayDeclaration,
            maybeImporterEoriNumberAnswer,
            maybeDeclarantEoriNumberAnswer,
            claimedReimbursementsAnswer,
            maybeReimbursementMethodAnswer,
            schedule,
            maybeAssociatedMRNs,
            maybeTypeOfClaim.getOrElse(Individual),
            maybeAssociatedMRNsClaimsAnswer
          )
        }.toEither
          .leftMap { errors =>
            Error(
              s"could not create complete claim in order to submit claim request: ${
                errors
                .map(_.toString)
                .toList
                .mkString("; ")}"
            )
          }

      case _ => Left(Error("unknown claim type"))
    }

  implicit val eq: Eq[CompleteClaim]         = Eq.fromUniversalEquals[CompleteClaim]
  implicit val format: Format[CompleteClaim] = Json.format[CompleteClaim]

  implicit class CompleteClaimOps(private val completeClaim: CompleteClaim) extends AnyVal {

    def bankDetails: Option[BankAccountDetails] =
      completeClaim.bankAccountDetailsAnswer match {
        case None =>
          for {
            displayDeclaration   <- completeClaim.displayDeclaration
            bankDetails          <- displayDeclaration.displayResponseDetail.maskedBankDetails
            consigneeBankDetails <- bankDetails.consigneeBankDetails
          } yield BankAccountDetails(
            AccountName(consigneeBankDetails.accountHolderName),
            SortCode(consigneeBankDetails.sortCode),
            AccountNumber(consigneeBankDetails.accountNumber)
          )
        case _    => completeClaim.bankAccountDetailsAnswer
      }

    def bankAccountType: String = BankAccountType.allAccountTypes.map(_.value).toString()
  }
}
