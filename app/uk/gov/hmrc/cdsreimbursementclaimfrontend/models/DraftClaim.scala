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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.DeclarantEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.ImporterEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyCodesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyTypesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementClaimAnswer

import java.util.UUID

final case class DraftClaim(
  id: UUID,
  selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer] = None,
  movementReferenceNumber: Option[MRN] = None,
  duplicateMovementReferenceNumberAnswer: Option[MRN] = None,
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
  reimbursementClaimAnswer: Option[ReimbursementClaimAnswer] = None,
  commoditiesDetailsAnswer: Option[CommodityDetails] = None,
  claimNorthernIrelandAnswer: Option[ClaimNorthernIrelandAnswer] = None,
  displayDeclaration: Option[DisplayDeclaration] = None,
  duplicateDisplayDeclaration: Option[DisplayDeclaration] = None,
  importerEoriNumberAnswer: Option[ImporterEoriNumber] = None,
  declarantEoriNumberAnswer: Option[DeclarantEoriNumber] = None,
  claimsAnswer: Option[ClaimsAnswer] = None,
  reimbursementMethodAnswer: Option[ReimbursementMethodAnswer] = None,
  scheduledDocumentAnswer: Option[ScheduledDocumentAnswer] = None,
  associatedMRNsAnswer: Option[AssociatedMRNsAnswer] = None,
  associatedMRNsDeclarationAnswer: Option[AssociatedMRNsDeclarationAnswer] = None,
  associatedMRNsDutiesSelectedAnswer: Option[AssociatedMRNsDutiesSelectedAnswer] = None
) {

  def isMandatoryContactDataAvailable: Boolean =
    (mrnContactAddressAnswer *> mrnContactDetailsAnswer).isDefined

  object MRNs {

    lazy val list: List[MRN] =
      leadMrn.toList ++ associatedMRNsAnswer.list

    def apply(): List[MRN] = list

    def leadMrn: Option[LeadMrn] = movementReferenceNumber

    def total: Total =
      (movementReferenceNumber *> Some(1)) |+| associatedMRNsAnswer.map(_.size) getOrElse 0

    def combineWithDeclarations: Seq[(MRN, DisplayDeclaration)] =
      (leadMrn, displayDeclaration).bisequence.toList ++
        (associatedMRNsAnswer.map(_.toList), associatedMRNsDeclarationAnswer.map(_.toList))
          .mapN((mrns, declarations) => mrns zip declarations)
          .getOrElse(Nil)

    def get(index: Int): Option[MRN] =
      list.get(index.toLong)
  }

  object DutiesSelections {

    lazy val list: List[List[Duty]] = {
      val x  = dutiesSelectedAnswer.map(_.toList).getOrElse(Nil)
      val xs = associatedMRNsDutiesSelectedAnswer
        .map(_.map(_.toList).toList)
        .getOrElse(Nil)
      x :: xs
    }

    def get(index: Int): Option[List[Duty]] =
      list.get(index.toLong)

  }

}

object DraftClaim {

  val blank: DraftClaim = DraftClaim(UUID.randomUUID())

  implicit val eq: Eq[DraftClaim]          = Eq.fromUniversalEquals
  implicit val format: OFormat[DraftClaim] = derived.oformat()
}
