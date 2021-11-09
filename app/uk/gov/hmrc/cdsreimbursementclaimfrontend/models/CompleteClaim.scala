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
  typeOfClaim: TypeOfClaimAnswer,
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

  lazy val bankDetails: Option[BankAccountDetails] =
    bankAccountDetailsAnswer match {
      case None =>
        for {
          declaration          <- displayDeclaration
          bankDetails          <- declaration.displayResponseDetail.maskedBankDetails
          consigneeBankDetails <- bankDetails.consigneeBankDetails
        } yield BankAccountDetails(
          AccountName(consigneeBankDetails.accountHolderName),
          SortCode(consigneeBankDetails.sortCode),
          AccountNumber(consigneeBankDetails.accountNumber)
        )
      case _    => bankAccountDetailsAnswer
    }

  lazy val bankAccountType: String =
    BankAccountType.allAccountTypes.map(_.value).toString()
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
            maybeClaimedReimbursementsAnswer,
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
          ClaimedReimbursementsAnswer.validator.validate(maybeClaimedReimbursementsAnswer),
          if (maybeTypeOfClaim.exists(_ === Scheduled))
            ScheduledDocumentAnswer.validator.validate(maybeScheduledDocument)
          else Valid(None)
        ).mapN { case (mrn, declaration, maybeDeclarant, maybeEvidences, maybeCommodity, maybeClaim, maybeSchedule) =>
          CompleteClaim(
            id,
            maybeTypeOfClaim.getOrElse(Individual),
            mrn,
            maybeDuplicateMovementReferenceNumberAnswer,
            maybeDeclarant,
            detailsRegisteredWithCds(maybeDeclarant, declaration, verifiedEmail),
            maybeDraftMrnContactDetails,
            maybeDraftMrnContactAddress,
            maybeBasisForClaim,
            maybeBankAccountDetails,
            maybeEvidences,
            maybeCommodity,
            maybeDraftNorthernIrelandAnswer,
            maybeDisplayDeclaration,
            maybeDuplicateDisplayDeclaration,
            maybeImporterEoriNumberAnswer,
            maybeDeclarantEoriNumberAnswer,
            maybeClaim,
            maybeReimbursementMethodAnswer,
            maybeSchedule,
            maybeAssociatedMRNs,
            maybeAssociatedMRNsClaimsAnswer
          )
        }.toEither
          .leftMap { errors =>
            Error(
              s"could not create complete claim in order to submit claim request: ${errors
                .map(_.toString)
                .toList
                .mkString("; ")}"
            )
          }

      case _ => Left(Error("unknown claim type"))
    }

  implicit val eq: Eq[CompleteClaim]         = Eq.fromUniversalEquals[CompleteClaim]
  implicit val format: Format[CompleteClaim] = Json.format[CompleteClaim]
}
