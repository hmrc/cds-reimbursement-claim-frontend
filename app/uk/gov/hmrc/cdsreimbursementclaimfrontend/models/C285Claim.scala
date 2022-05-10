/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import java.util.UUID
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType.Consignee
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType.User

final case class C285Claim(
  id: UUID,
  typeOfClaim: TypeOfClaimAnswer,
  movementReferenceNumber: MRN,
  duplicateMovementReferenceNumberAnswer: Option[MRN],
  declarantTypeAnswer: DeclarantTypeAnswer,
  detailsRegisteredWithCdsAnswer: DetailsRegisteredWithCdsAnswer,
  mrnContactDetailsAnswer: Option[MrnContactDetails],
  mrnContactAddressAnswer: Option[ContactAddress],
  basisOfClaimAnswer: BasisOfClaimAnswer,
  bankAccountDetailsAnswer: Option[BankAccountDetails],
  documents: NonEmptyList[EvidenceDocument],
  additionalDetailsAnswer: AdditionalDetailsAnswer,
  displayDeclaration: Option[DisplayDeclaration],
  duplicateDisplayDeclaration: Option[DisplayDeclaration],
  importerEoriNumber: Option[ImporterEoriNumberAnswer],
  declarantEoriNumber: Option[DeclarantEoriNumberAnswer],
  claimedReimbursementsAnswer: ClaimedReimbursementsAnswer,
  reimbursementMethodAnswer: ReimbursementMethodAnswer,
  associatedMRNsAnswer: Option[AssociatedMRNsAnswer],
  associatedMRNsClaimsAnswer: Option[AssociatedMRNsClaimsAnswer]
) {

  lazy val totalReimbursementAmount: BigDecimal =
    typeOfClaim match {
      case Individual => claimedReimbursementsAnswer.total
      case Scheduled  => claimedReimbursementsAnswer.total
      case Multiple   => multipleReimbursementsTotal
    }

  lazy val bankDetails: Option[BankAccountDetails] =
    bankAccountDetailsAnswer match {
      case None =>
        for {
          declaration          <- displayDeclaration
          bankDetails          <- declaration.displayResponseDetail.maskedBankDetails
          consigneeBankDetails <- bankDetails.consigneeBankDetails
        } yield consigneeBankDetails
      case _    => bankAccountDetailsAnswer
    }

  lazy val bankAccountType: String =
    BankAccountType.values.map(_.value).toString()

  def multipleReimbursementsTotal: BigDecimal =
    (claimedReimbursementsAnswer.toList ++ associatedMRNsClaimsAnswer.toList.flatMap(_.toList).flatMap(_.toList))
      .map(_.claimAmount)
      .sum
}

object C285Claim {

  def fromDraftClaim(draftClaim: DraftClaim, verifiedEmail: Email, userEori: Eori): Either[Error, C285Claim] = {
    val declarant: DeclarantTypeAnswer =
      draftClaim.getClaimantType(userEori) match {
        case Consignee => DeclarantTypeAnswer.Importer
        case Declarant => DeclarantTypeAnswer.AssociatedWithRepresentativeCompany
        case User      => DeclarantTypeAnswer.AssociatedWithRepresentativeCompany
      }
    draftClaim match {
      case DraftClaim(
            id,
            maybeTypeOfClaim,
            maybeMrn,
            maybeDuplicateMovementReferenceNumberAnswer,
            maybeDraftMrnContactDetails,
            maybeDraftMrnContactAddress,
            maybeBankAccountDetails,
            _,
            maybeBasisForClaim,
            _,
            maybeSupportingEvidences,
            _,
            maybeDraftAdditionalAnswer,
            _,
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
            _,
            _
          ) =>
        (
          MRN.validator.validate(maybeMrn),
          BasisOfClaimAnswer.validator.validate(maybeBasisForClaim),
          DisplayDeclaration.validator.validate(maybeDisplayDeclaration),
          AdditionalDetailsAnswer.validator.validate(maybeDraftAdditionalAnswer),
          SupportingEvidencesAnswer.validator.validate(maybeSupportingEvidences),
          ClaimedReimbursementsAnswer.validator.validate(maybeClaimedReimbursementsAnswer),
          if (maybeTypeOfClaim.exists(_ === Scheduled))
            ScheduledDocumentAnswer.validator.validate(maybeScheduledDocument)
          else Valid(None)
        ).mapN {
          case (
                mrn,
                basisOfClaims,
                declaration,
                additional,
                evidences,
                claim,
                maybeSchedule
              ) =>
            C285Claim(
              id,
              maybeTypeOfClaim.getOrElse(Individual),
              mrn,
              maybeDuplicateMovementReferenceNumberAnswer,
              declarant,
              DetailsRegisteredWithCdsAnswer(declarant, declaration, verifiedEmail),
              maybeDraftMrnContactDetails,
              maybeDraftMrnContactAddress,
              basisOfClaims,
              maybeBankAccountDetails,
              (evidences ++ maybeSchedule.map(_.uploadDocument).toList).map(EvidenceDocument.from),
              additional,
              maybeDisplayDeclaration,
              maybeDuplicateDisplayDeclaration,
              maybeImporterEoriNumberAnswer,
              maybeDeclarantEoriNumberAnswer,
              claim,
              maybeReimbursementMethodAnswer.getOrElse(ReimbursementMethodAnswer.BankAccountTransfer),
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
  }

  implicit val eq: Eq[C285Claim]         = Eq.fromUniversalEquals[C285Claim]
  implicit val format: Format[C285Claim] = Json.format[C285Claim]
}
