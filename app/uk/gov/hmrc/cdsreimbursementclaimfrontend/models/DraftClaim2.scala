package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import java.util.UUID

object Serge {

  sealed abstract class Ref(value: String)
  final case class MRN(value: String) extends Ref(value)
  final case class EntryNumber(value: String) extends Ref(value)

  case class DraftClaim[A <: Ref](
    // EORI details
    // Type of claim
    // Supporting Evidence Journey
    // Claiming of duties journey
    // CYA (end)
    // Confirmation page
    bankAccountDetailsAnswer: Option[BankAccountDetailsAnswer],
    contactDetailsAnswer: Option[ContactDetailsAnswer],
    declarantTypeAnswer: Option[DeclarantTypeAnswer],
    detailsRegisteredWithCdsAnswer: Option[DetailsRegisteredWithCdsAnswer],
    claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer],
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer],
    movementReferenceNumber: Option[MovementReferenceNumber],
    supportingEvidenceAnswer: Option[SupportingEvidenceAnswer],
    dutiesSelectedAnswer: Option[DutiesSelectedAnswer],
    commoditiesDetailsAnswer: Option[CommoditiesDetailsAnswer],
    claimsAnswer: Option[ClaimsAnswer],
    importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer],
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer]
  )

  final case class MrnClaim(
    duplicateMovementReferenceNumberAnswer: Option[DuplicateMovementReferenceNumberAnswer],
    displayDeclaration: Option[DisplayDeclaration],
    duplicateDisplayDeclaration: Option[DisplayDeclaration],
    basisOfClaimAnswer: Option[BasisOfClaimAnswer]
  ) extends DraftClaim[MRN]

  final case class EntryNumberClaim(
    declarationDetailsAnswer: Option[DeclarationDetailsAnswer],
    duplicateDeclarationDetailsAnswer: Option[DuplicateDeclarationDetailsAnswer],
    reasonForBasisAndClaimAnswer: Option[ReasonAndBasisOfClaimAnswer]
  ) extends DraftClaim[EntryNumber]

  final case class MrnClaimBulk(
    extraPage : ???
                               ) extend ???

  final case class DraftBulkClaim(
    leadReferenceNumber : ReferenceNumber,
    referenceNumbers : Set[ReferenceNumber],


                       )

  final case class DraftBulkScheduleClaim(

                                         )

  case class Bulk(lead: MRNClaim, rest: MRNClaim)

}

sealed abstract class ReferenceNumber extends Product with Serializable {
  val value: String
}

object ReferenceNumber {
  final case class MRN(value: String) extends ReferenceNumber
  final case class ERN(value: String) extends ReferenceNumber
}

sealed trait DraftClaim2[A <: ReferenceNumber] extends Product with Serializable {
  val id: UUID
  val referenceNumber: ReferenceNumber
  // specify all the answers which are common to all journey types
  // e.g. claiming duties, supporting evidence

}

object DraftClaim2 {

  final case class DraftC285MRN(
    id: UUID
  ) extends DraftClaim2[ReferenceNumber.MRN]

  final case class DraftC285ERN(
    id: UUID
  ) extends DraftClaim2[ReferenceNumber.ERN]

  final case class DraftC285BulkClaim(
    id: UUID
  ) extends DraftClaim2[ReferenceNumber.MRN]

  final case class DraftC285BulkClaim(
    id: UUID
  ) extends DraftClaim2[ReferenceNumber.ERN]

}
