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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model

import cats.Eq
//import julienrf.json.derived
//import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, DutiesSelectedAnswer, SupportingEvidenceAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.ReferenceNumber.{EntryNumber, MRN}

import java.util.UUID

sealed trait DraftClaim[T <: ReferenceNumber] extends Claim[T] {
  val movementReferenceNumber: Option[T]
  val bankAccountDetailsAnswer: Option[BankAccountDetailsAnswer]
  val contactDetailsAnswer: Option[ContactDetailsAnswer]
  val declarantTypeAnswer: Option[DeclarantTypeAnswer]
  val detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer]
  val claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer]
  val selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer]
  val supportingEvidenceAnswer: Option[SupportingEvidenceAnswer]
  val dutiesSelectedAnswer: Option[DutiesSelectedAnswer]
  val commoditiesDetailsAnswer: Option[CommoditiesDetailsAnswer]
  val claimsAnswer: Option[ClaimsAnswer]
  val importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer]
  val declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer]
}

object DraftClaim {

  final case class MrnDraftClaim(
    id: UUID,
    movementReferenceNumber: Option[MRN] = None,
    bankAccountDetailsAnswer: Option[BankAccountDetailsAnswer] = None,
    contactDetailsAnswer: Option[ContactDetailsAnswer] = None,
    declarantTypeAnswer: Option[DeclarantTypeAnswer] = None,
    detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer] = None,
    claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer] = None,
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer] = None,
    supportingEvidenceAnswer: Option[SupportingEvidenceAnswer] = None,
    dutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    commoditiesDetailsAnswer: Option[CommoditiesDetailsAnswer] = None,
    claimsAnswer: Option[ClaimsAnswer] = None,
    importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer] = None,
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer] = None,
    duplicateMovementReferenceNumberAnswer: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    duplicateDisplayDeclaration: Option[DisplayDeclaration] = None,
    basisOfClaimAnswer: Option[BasisOfClaimAnswer] = None
  ) extends DraftClaim[MRN]

  final case class EntryNumberDraftClaim(
    id: UUID,
    movementReferenceNumber: Option[EntryNumber] = None,
    bankAccountDetailsAnswer: Option[BankAccountDetailsAnswer] = None,
    contactDetailsAnswer: Option[ContactDetailsAnswer] = None,
    declarantTypeAnswer: Option[DeclarantTypeAnswer] = None,
    detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer] = None,
    claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer] = None,
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer] = None,
    supportingEvidenceAnswer: Option[SupportingEvidenceAnswer] = None,
    dutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    commoditiesDetailsAnswer: Option[CommoditiesDetailsAnswer] = None,
    claimsAnswer: Option[ClaimsAnswer] = None,
    importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer] = None,
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer] = None,
    declarationDetailsAnswer: Option[DeclarationDetailsAnswer] = None,
    duplicateDeclarationDetailsAnswer: Option[DuplicateDeclarationDetailsAnswer] = None,
    reasonForBasisAndClaimAnswer: Option[ReasonAndBasisOfClaimAnswer] = None
  ) extends DraftClaim[EntryNumber]

  implicit def eq[T <: ReferenceNumber]: Eq[DraftClaim[T]] = Eq.fromUniversalEquals
  //implicit def format[T <: ReferenceNumber]: OFormat[DraftClaim[T]] = derived.oformat()
}
