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
import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.Declaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswers

import java.util.UUID

sealed trait DraftClaim extends Product with Serializable {
  val id: UUID
}

object DraftClaim {

  final case class DraftC285Claim(
    id: UUID,
    movementReferenceNumberAnswer: Option[MovementReferenceNumberAnswer],
    duplicateMovementReferenceNumberAnswer: Option[DuplicateMovementReferenceNumberAnswer],
    declarationDetailAnswers: Option[DeclarationDetailAnswers],
    duplicateDeclarationDetailAnswers: Option[DuplicateDeclarantDetailAnswers],
    declarantTypeAnswer: Option[DeclarantTypeAnswer],
    claimantDetailsAsIndividualAnswers: Option[ClaimantDetailsAsIndividualAnswer],
    claimantDetailsAsImporterCompanyAnswers: Option[ClaimantDetailsAsImporterCompanyAnswer],
    bankAccountDetailsAnswers: Option[BankAccountDetailsAnswers],
    reasonForClaim: Option[ReasonForClaimAnswer],
    supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
    ukDutyAmountAnswers: Option[UKDutyAmountAnswers],
    euDutyAmountAnswers: Option[EuDutyAmountAnswers],
    claimAnswers: Option[ClaimAnswers],
    commoditiesDetailsAnswers: Option[CommoditiesDetailsAnswers],
    reasonForBasisAndClaimAnswer: Option[ReasonForClaimAndBasisAnswer],
    maybeDeclaration: Option[Declaration], //Data that has come back from ACC-14
    maybeDuplicateDeclaration: Option[Declaration],
    importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer],
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer]
  ) extends DraftClaim

  object DraftC285Claim {
    implicit val eq: Eq[DraftC285Claim]          = Eq.fromUniversalEquals[DraftC285Claim]
    val newDraftC285Claim: DraftC285Claim        =
      DraftC285Claim(
        UUID.randomUUID(),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
    implicit val format: OFormat[DraftC285Claim] = Json.format[DraftC285Claim]
  }

  implicit class DraftClaimOps(private val draftClaim: DraftClaim) extends AnyVal {
    def fold[A](
      draftC285Claim: DraftC285Claim => A
    ): A =
      draftClaim match {
        case a: DraftC285Claim => draftC285Claim(a)
      }

    def declarantType: Option[DeclarantType] = draftClaim match {
      case DraftC285Claim(
            _,
            _,
            _,
            _,
            _,
            declarantTypeAnswer,
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
        declarantTypeAnswer match {
          case Some(value) =>
            value match {
              case DeclarantTypeAnswer.IncompleteDeclarantTypeAnswer(declarantType) => declarantType
              case DeclarantTypeAnswer.CompleteDeclarantTypeAnswer(declarantType)   => Some(declarantType)
            }
          case None        => None
        }
    }

    def movementReferenceNumber: Option[Either[EntryNumber, MRN]] = draftClaim match {
      case DraftC285Claim(
            _,
            movementReferenceNumberAnswer,
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
        movementReferenceNumberAnswer match {
          case Some(movementReferenceNumberAnswer) =>
            movementReferenceNumberAnswer match {
              case MovementReferenceNumberAnswer.IncompleteMovementReferenceNumberAnswer(movementReferenceNumber) =>
                movementReferenceNumber
              case MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer(movementReferenceNumber)   =>
                Some(movementReferenceNumber)
            }
          case None                                => None
        }
    }
  }

  implicit val eq: Eq[DraftClaim]          = Eq.fromUniversalEquals
  implicit val format: OFormat[DraftClaim] = derived.oformat()

}
