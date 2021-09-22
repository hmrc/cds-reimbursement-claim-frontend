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
import cats.syntax.all._
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.CheckDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.CheckClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyCodesAnswer, DutyPaidAndClaimAmountAnswer, DutyTypesAnswer}

import java.util.UUID

sealed trait DraftClaim extends Product with Serializable {
  val id: UUID
  def isMandatoryDataAvailable: Boolean
}

object DraftClaim {

  final case class DraftC285Claim(
    id: UUID,
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer] = None,
    movementReferenceNumber: Option[MovementReferenceNumber] = None,
    duplicateMovementReferenceNumberAnswer: Option[MovementReferenceNumber] = None,
    declarantTypeAnswer: Option[DeclarantTypeAnswer] = None,
    detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer] = None,
    mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
    mrnContactAddressAnswer: Option[ContactAddress] = None,
    bankAccountDetailsAnswer: Option[BankAccountDetails] = None,
    bankAccountTypeAnswer: Option[BankAccountType] = None,
    basisOfClaimAnswer: Option[BasisOfClaim] = None,
    supportingEvidencesAnswer: Option[SupportingEvidencesAnswer] = None,
    dutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    dutyTypesSelectedAnswer: Option[DutyTypesAnswer] = None,
    dutyCodesSelectedAnswer: Option[DutyCodesAnswer] = None,
    dutyPaidAndClaimAmountAnswer: Option[DutyPaidAndClaimAmountAnswer] = None,
    commoditiesDetailsAnswer: Option[CommodityDetails] = None,
    claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    duplicateDisplayDeclaration: Option[DisplayDeclaration] = None,
    importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer] = None,
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer] = None,
    claimsAnswer: Option[ClaimsAnswer] = None,
    checkClaimAnswer: Option[CheckClaimAnswer] = None,
    checkDeclarationDetailsAnswer: Option[CheckDeclarationDetailsAnswer] = None,
    scheduledDocumentAnswer: Option[ScheduledDocumentAnswer] = None,
    associatedMovementReferenceNumbersAnswer: Option[AssociatedMRNsAnswer] = None
  ) extends DraftClaim {

    def isMandatoryDataAvailable: Boolean =
      (mrnContactAddressAnswer *> mrnContactDetailsAnswer).isDefined
  }

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

    def detailsRegisteredWithCds: Option[DetailsRegisteredWithCdsAnswer] = draftClaim match {
      case dc: DraftC285Claim => dc.detailsRegisteredWithCdsAnswer
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
