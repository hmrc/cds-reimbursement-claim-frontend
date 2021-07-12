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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.CheckDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.CheckClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.DetailsRegisteredWithCdsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.{CompleteDetailsRegisteredWithCdsAnswer, IncompleteDetailsRegisteredWithCdsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{DutiesSelectedAnswer, SupportingEvidenceAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimsAnswer

import java.util.UUID

sealed trait DraftClaim extends Product with Serializable {
  val id: UUID
}

object DraftClaim {

  final case class DraftC285Claim(
                                   id: UUID,
                                   selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer],
                                   movementReferenceNumber: Option[MovementReferenceNumber],
                                   duplicateMovementReferenceNumberAnswer: Option[MovementReferenceNumber],
                                   declarationDetailsAnswer: Option[DeclarationDetails],
                                   duplicateDeclarationDetailsAnswer: Option[DuplicateDeclarationDetailsAnswer],
                                   declarantTypeAnswer: Option[DeclarantTypeAnswer],
                                   detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer],
                                   contactDetailsAnswer: Option[ContactDetailsAnswer],
                                   bankAccountDetailsAnswer: Option[BankAccountDetailsAnswer],
                                   basisOfClaimAnswer: Option[BasisOfClaimAnswer],
                                   supportingEvidenceAnswer: Option[SupportingEvidenceAnswer],
                                   dutiesSelectedAnswer: Option[DutiesSelectedAnswer],
                                   commoditiesDetailsAnswer: Option[CommodityDetails],
                                   claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer],
                                   reasonForBasisAndClaimAnswer: Option[ReasonAndBasisOfClaimAnswer],
                                   displayDeclaration: Option[DisplayDeclaration],
                                   duplicateDisplayDeclaration: Option[DisplayDeclaration],
                                   importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer],
                                   declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer],
                                   claimsAnswer: Option[ClaimsAnswer],
                                   checkClaimAnswer: Option[CheckClaimAnswer],
                                   checkDeclarationDetailsAnswer: Option[CheckDeclarationDetailsAnswer]
  ) extends DraftClaim

  object DraftC285Claim {
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
        None,
        None,
        None,
        None
      )
    implicit val eq: Eq[DraftC285Claim]          = Eq.fromUniversalEquals[DraftC285Claim]
    implicit val format: OFormat[DraftC285Claim] = Json.format[DraftC285Claim]
  }

  implicit class DraftClaimOps(private val draftClaim: DraftClaim) extends AnyVal {
    def fold[A](
      draftC285Claim: DraftC285Claim => A
    ): A =
      draftClaim match {
        case a: DraftC285Claim => draftC285Claim(a)
      }

    def isMrnFlow: Boolean =
      draftClaim.movementReferenceNumber
        .fold(sys.error("no movement or entry reference number found"))(_.isRight)

    def detailsRegisteredWithCds: Option[DetailsRegisteredWithCdsFormData] = draftClaim match {
      case dc: DraftC285Claim =>
        dc.detailsRegisteredWithCdsAnswer match {
          case Some(answer) =>
            answer match {
              case complete: CompleteDetailsRegisteredWithCdsAnswer     => Some(complete.detailsRegisteredWithCds)
              case incomplete: IncompleteDetailsRegisteredWithCdsAnswer => incomplete.detailsRegisteredWithCds
            }
          case None         => None
        }
      case _                  => None
    }

    def declarantType: Option[DeclarantType] = draftClaim match {
      case draftC285Claim: DraftC285Claim =>
        draftC285Claim.declarantTypeAnswer match {
          case Some(value) =>
            value match {
              case DeclarantTypeAnswer.IncompleteDeclarantTypeAnswer(declarantType) => declarantType
              case DeclarantTypeAnswer.CompleteDeclarantTypeAnswer(declarantType)   => Some(declarantType)
            }
          case None        => None
        }
    }

    def movementReferenceNumber: Option[Either[EntryNumber, MRN]] = draftClaim match {
      case draftC285Claim: DraftC285Claim =>
        draftC285Claim.movementReferenceNumber.map(_.value)
    }
  }

  implicit val eq: Eq[DraftClaim]          = Eq.fromUniversalEquals
  implicit val format: OFormat[DraftClaim] = derived.oformat()

}
