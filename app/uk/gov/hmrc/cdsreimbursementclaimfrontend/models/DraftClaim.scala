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
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckClaimantDetailsController.CheckClaimantDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.CheckDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.CheckClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.DetailsRegisteredWithCdsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.{CompleteDetailsRegisteredWithCdsAnswer, IncompleteDetailsRegisteredWithCdsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}

import java.util.UUID

sealed trait DraftClaim extends Product with Serializable {
  val id: UUID
}

object DraftClaim {

  final case class DraftC285Claim(
    id: UUID,
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer] = None,
    movementReferenceNumber: Option[MovementReferenceNumber] = None,
    duplicateMovementReferenceNumberAnswer: Option[MovementReferenceNumber] = None,
    declarationDetailsAnswer: Option[DeclarationDetailsAnswer] = None,
    duplicateDeclarationDetailsAnswer: Option[DuplicateDeclarationDetailsAnswer] = None,
    declarantTypeAnswer: Option[DeclarantTypeAnswer] = None,
    checkClaimantDetailsAnswer: Option[CheckClaimantDetailsAnswer] = None,
    detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer] = None,
    contactDetailsAnswer: Option[ContactDetailsAnswer] = None,
    mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
    mrnContactAddressAnswer: Option[NonUkAddress] = None,
    bankAccountDetailsAnswer: Option[BankAccountDetailsAnswer] = None,
    basisOfClaimAnswer: Option[BasisOfClaim] = None,
    supportingEvidencesAnswer: Option[SupportingEvidencesAnswer] = None,
    dutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    commoditiesDetailsAnswer: Option[CommodityDetails] = None,
    claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer] = None,
    reasonForBasisAndClaimAnswer: Option[ReasonAndBasisOfClaimAnswer] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    duplicateDisplayDeclaration: Option[DisplayDeclaration] = None,
    importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer] = None,
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer] = None,
    claimsAnswer: Option[ClaimsAnswer] = None,
    checkClaimAnswer: Option[CheckClaimAnswer] = None,
    checkDeclarationDetailsAnswer: Option[CheckDeclarationDetailsAnswer] = None,
    scheduledDocumentAnswer: Option[ScheduledDocumentAnswer] = None
  ) extends DraftClaim

  object DraftC285Claim {
    val newDraftC285Claim: DraftC285Claim = DraftC285Claim(UUID.randomUUID())

    implicit val eq: Eq[DraftC285Claim] = Eq.fromUniversalEquals[DraftC285Claim]
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

    def declarantType: Option[DeclarantTypeAnswer] = draftClaim match {
      case draftC285Claim: DraftC285Claim => draftC285Claim.declarantTypeAnswer
    }

    def movementReferenceNumber: Option[Either[EntryNumber, MRN]] = draftClaim match {
      case draftC285Claim: DraftC285Claim =>
        draftC285Claim.movementReferenceNumber.map(_.value)
    }
  }

  implicit val eq: Eq[DraftClaim]          = Eq.fromUniversalEquals
  implicit val format: OFormat[DraftClaim] = derived.oformat()

}
