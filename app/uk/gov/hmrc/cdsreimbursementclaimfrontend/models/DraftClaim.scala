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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyCodesAnswer, DutyPaidAndClaimAmountAnswer, DutyTypesAnswer, ReimbursementMethodAnswer}
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
    importerEoriNumberAnswer: Option[ImporterEoriNumber] = None,
    declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer] = None,
    claimsAnswer: Option[ClaimsAnswer] = None,
    checkClaimAnswer: Option[CheckClaimAnswer] = None,
    checkDeclarationDetailsAnswer: Option[CheckDeclarationDetailsAnswer] = None,
    scheduledDocumentAnswer: Option[ScheduledDocumentAnswer] = None,
    associatedMRNsAnswer: Option[AssociatedMRNsAnswer] = None,
    associatedMRNsDeclarationAnswer: Option[AssociatedMRNsDeclarationAnswer] = None,
    reimbursementMethodAnswer: Option[ReimbursementMethodAnswer] = None
  ) extends DraftClaim {

    def isMrnFlow: Boolean =
      movementReferenceNumber.exists(_.value.isRight)

    def isMandatoryContactDataAvailable: Boolean =
      (mrnContactAddressAnswer *> mrnContactDetailsAnswer).isDefined

    object MRNs {

      def leadMrn: Option[LeadMrn] =
        movementReferenceNumber.flatMap(_.value.toOption)

      def apply(): List[MRN] =
        leadMrn.toList ++ associatedMRNsAnswer.list

      def total: Total =
        (movementReferenceNumber *> Some(1)) |+| associatedMRNsAnswer.map(_.size) getOrElse 0

      def combineWithDeclarations: Seq[(MRN, DisplayDeclaration)] =
        (leadMrn, displayDeclaration).bisequence.toList ++
          (associatedMRNsAnswer.map(_.toList), associatedMRNsDeclarationAnswer.map(_.toList))
            .mapN((mrns, declarations) => mrns zip declarations)
            .getOrElse(Nil)
    }
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
  }

  implicit val eq: Eq[DraftClaim]          = Eq.fromUniversalEquals
  implicit val format: OFormat[DraftClaim] = derived.oformat()

  implicit def claimToC285Claim(draftClaim: DraftClaim): DraftC285Claim =
    draftClaim fold identity
}
