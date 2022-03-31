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
import cats.syntax.all._
import julienrf.json.derived
import play.api.Logger
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import java.util.UUID

final case class DraftClaim(
  id: UUID,
  typeOfClaim: Option[TypeOfClaimAnswer] = None,
  movementReferenceNumber: Option[MRN] = None,
  duplicateMovementReferenceNumberAnswer: Option[MRN] = None,
  mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
  mrnContactAddressAnswer: Option[ContactAddress] = None,
  bankAccountDetailsAnswer: Option[BankAccountDetails] = None,
  bankAccountTypeAnswer: Option[BankAccountType] = None,
  basisOfClaimAnswer: Option[BasisOfClaimAnswer] = None,
  supportingEvidencesAnswer: Option[SupportingEvidencesAnswer] = None,
  dutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
  commoditiesDetailsAnswer: Option[CommodityDetailsAnswer] = None,
  whetherNorthernIrelandAnswer: Option[YesNo] = None,
  displayDeclaration: Option[DisplayDeclaration] = None,
  duplicateDisplayDeclaration: Option[DisplayDeclaration] = None,
  importerEoriNumberAnswer: Option[ImporterEoriNumberAnswer] = None,
  declarantEoriNumberAnswer: Option[DeclarantEoriNumberAnswer] = None,
  claimedReimbursementsAnswer: Option[ClaimedReimbursementsAnswer] = None,
  reimbursementMethodAnswer: Option[ReimbursementMethodAnswer] = None,
  scheduledDocumentAnswer: Option[ScheduledDocumentAnswer] = None,
  associatedMRNsAnswer: Option[AssociatedMRNsAnswer] = None,
  associatedMRNsDeclarationAnswer: Option[AssociatedMRNsDeclarationAnswer] = None,
  associatedMRNsDutiesSelectedAnswer: Option[AssociatedMRNsDutiesSelectedAnswer] = None,
  associatedMRNsClaimsAnswer: Option[AssociatedMRNsClaimsAnswer] = None,
  selectedDutyTaxCodesReimbursementAnswer: Option[SelectedDutyTaxCodesReimbursementAnswer] = None
) {

  lazy val multipleClaimsAnswer: List[(MRN, ClaimedReimbursementsAnswer)] = {
    val mrns   = MRNs()
    val claims = Claims()
    mrns zip claims
  }

  def isMandatoryContactDataAvailable: Boolean =
    (mrnContactAddressAnswer *> mrnContactDetailsAnswer).isDefined

  def hasNorthernIrelandBasisOfClaim: Boolean =
    basisOfClaimAnswer.exists(BasisOfClaims.northernIreland.contains(_))

  def findNonEmptyBankAccountDetails: Option[BankAccountDetails] =
    Stream(
      bankAccountDetailsAnswer,
      displayDeclaration.flatMap(_.displayResponseDetail.maskedBankDetails.flatMap(_.consigneeBankDetails)),
      displayDeclaration.flatMap(_.displayResponseDetail.maskedBankDetails.flatMap(_.declarantBankDetails))
    ).find(_.nonEmpty).flatten

  object MRNs extends DraftClaim.LeadAndAssociatedItems(movementReferenceNumber, associatedMRNsAnswer) {

    def apply(): List[MRN] = list

    def leadMrn: Option[LeadMrn] = movementReferenceNumber

    def total: Total =
      (movementReferenceNumber *> Some(1)) |+| associatedMRNsAnswer.map(_.size) getOrElse 0

    def combineWithDeclarations: Seq[(MRN, DisplayDeclaration)] =
      (leadMrn, displayDeclaration).bisequence.toList ++
        (associatedMRNsAnswer.map(_.toList), associatedMRNsDeclarationAnswer.map(_.toList))
          .mapN((mrns, declarations) => mrns zip declarations)
          .getOrElse(Nil)
  }

  object Declarations
      extends DraftClaim.LeadAndAssociatedItems[DisplayDeclaration](displayDeclaration, associatedMRNsDeclarationAnswer)

  object DutiesSelections
      extends DraftClaim.LeadAndAssociatedItemList[Duty](dutiesSelectedAnswer, associatedMRNsDutiesSelectedAnswer)

  object Claims
      extends DraftClaim.LeadAndAssociatedItemList[ClaimedReimbursement](
        claimedReimbursementsAnswer,
        associatedMRNsClaimsAnswer
      ) {

    def apply(): Seq[ClaimedReimbursementsAnswer] =
      claimedReimbursementsAnswer.toList ++ associatedMRNsClaimsAnswer.toList.flatMap(_.toList)
  }

  def extractEstablishmentAddress(user: SignedInUserDetails): Option[EstablishmentAddress] =
    displayDeclaration.flatMap { declaration =>
      getClaimantType(user.eori) match {
        case ClaimantType.Consignee                     =>
          declaration.displayResponseDetail.consigneeDetails.map(_.establishmentAddress)
        case ClaimantType.Declarant | ClaimantType.User =>
          Some(declaration.displayResponseDetail.declarantDetails.establishmentAddress)
      }
    }

  def isComplete: Boolean = {

    def findBankAccountDetails = Stream(
      bankAccountDetailsAnswer,
      displayDeclaration.flatMap(_.displayResponseDetail.bankDetails.flatMap(_.consigneeBankDetails)),
      displayDeclaration.flatMap(_.displayResponseDetail.bankDetails.flatMap(_.declarantBankDetails))
    ).find(_.nonEmpty).flatten

    def isSingleJourneyComplete: Boolean =
      SupportingEvidencesAnswer.validator.validate(supportingEvidencesAnswer).isValid &&
        BankAccountDetails.validator.validate(findBankAccountDetails).isValid &&
        ClaimedReimbursementsAnswer.validator.validate(claimedReimbursementsAnswer).isValid &&
        CommodityDetailsAnswer.validator.validate(commoditiesDetailsAnswer).isValid &&
        BasisOfClaimAnswer.validator.validate(basisOfClaimAnswer).isValid &&
        DisplayDeclaration.validator.validate(displayDeclaration).isValid &&
        MRN.validator.validate(movementReferenceNumber).isValid

    def isMultipleJourneyComplete: Boolean =
      SupportingEvidencesAnswer.validator.validate(supportingEvidencesAnswer).isValid &&
        BankAccountDetails.validator.validate(findBankAccountDetails).isValid &&
        CommodityDetailsAnswer.validator.validate(commoditiesDetailsAnswer).isValid &&
        BasisOfClaimAnswer.validator.validate(basisOfClaimAnswer).isValid &&
        AssociatedMRNsClaimsAnswer.validator.validate(associatedMRNsClaimsAnswer).isValid &&
        AssociatedMRNsAnswer.validator.validate(associatedMRNsAnswer).isValid &&
        DisplayDeclaration.validator.validate(displayDeclaration).isValid &&
        MRN.validator.validate(movementReferenceNumber).isValid

    def isScheduledJourneyComplete: Boolean =
      SupportingEvidencesAnswer.validator.validate(supportingEvidencesAnswer).isValid &&
        BankAccountDetails.validator.validate(findBankAccountDetails).isValid &&
        ClaimedReimbursementsAnswer.validator.validate(claimedReimbursementsAnswer).isValid &&
        CommodityDetailsAnswer.validator.validate(commoditiesDetailsAnswer).isValid &&
        BasisOfClaimAnswer.validator.validate(basisOfClaimAnswer).isValid &&
        ScheduledDocumentAnswer.validator.validate(scheduledDocumentAnswer).isValid &&
        DisplayDeclaration.validator.validate(displayDeclaration).isValid &&
        MRN.validator.validate(movementReferenceNumber).isValid

    typeOfClaim match {
      case Some(Scheduled) => isScheduledJourneyComplete
      case Some(Multiple)  => isMultipleJourneyComplete
      case _               => isSingleJourneyComplete
    }
  }

  def computeContactDetails(user: SignedInUserDetails): Option[MrnContactDetails] =
    (
      mrnContactDetailsAnswer,
      displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)),
      displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails)
    ) match {
      case (details @ Some(_), _, _) =>
        details

      case (_, Some(consigneeContactDetails), _) if getConsigneeEoriFromACC14.contains(user.eori) =>
        Some(
          MrnContactDetails(
            consigneeContactDetails.contactName.getOrElse(""),
            consigneeContactDetails.emailAddress
              .fold(user.email.getOrElse(Email("")))(address => Email(address)),
            consigneeContactDetails.telephone.map(PhoneNumber(_))
          )
        )

      case (_, None, _) if getConsigneeEoriFromACC14.contains(user.eori) =>
        Some(
          MrnContactDetails(
            user.contactName.value,
            user.email.getOrElse(Email("")),
            None
          )
        )

      case (_, _, Some(declarantContactDetails)) =>
        Some(
          MrnContactDetails(
            declarantContactDetails.contactName.getOrElse(""),
            declarantContactDetails.emailAddress
              .fold(user.email.getOrElse(Email("")))(address => Email(address)),
            declarantContactDetails.telephone.map(PhoneNumber(_))
          )
        )

      case (a, b, c) =>
        Logger(getClass).warn(
          s"Cannot compute contact details for MRN ${displayDeclaration
            .map(_.displayResponseDetail.declarationId)
            .getOrElse("<none>")} because of: hasPreviousAnswer=${a.isDefined}, hasConsigneeContactDetails=${b.isDefined}, hasDeclarantContactDetails=${c.isDefined}, userEoriMatchesConsigneeEori=${getConsigneeEoriFromACC14
            .contains(user.eori)}"
        )
        None
    }

  def computeAddressDetails(user: SignedInUserDetails): Option[ContactAddress] = (
    mrnContactAddressAnswer,
    displayDeclaration.flatMap(_.getConsigneeDetails),
    displayDeclaration.map(_.getDeclarantDetails)
  ) match {
    case (contactAddress @ Some(_), _, _) =>
      contactAddress

    case (None, Some(consigneeDetails), _) if getConsigneeEoriFromACC14.contains(user.eori) =>
      Some(consigneeDetails.establishmentAddress.toContactAddress)

    case (None, _, Some(declarantDetails)) =>
      Some(declarantDetails.establishmentAddress.toContactAddress)

    case (a, b, c) =>
      Logger(getClass).warn(
        s"Cannot compute address details for MRN ${displayDeclaration
          .map(_.displayResponseDetail.declarationId)
          .getOrElse("<none>")} because of: hasPreviousAnswer=${a.isDefined}, hasConsigneeContactDetails=${b.isDefined}, hasDeclarantContactDetails=${c.isDefined}, userEoriMatchesConsigneeEori=${getConsigneeEoriFromACC14
          .contains(user.eori)}"
      )
      None
  }

  def getDeclarantEoriFromACC14: Option[Eori] =
    displayDeclaration.map(_.getDeclarantEori)

  def getConsigneeEoriFromACC14: Option[Eori] =
    displayDeclaration.flatMap(_.getConsigneeEori)

  def getClaimantEori(userEoriNumber: Eori): Eori = getClaimantType(userEoriNumber) match {
    case ClaimantType.Consignee => getConsigneeEoriFromACC14.getOrElse(userEoriNumber)
    case ClaimantType.Declarant => getDeclarantEoriFromACC14.getOrElse(userEoriNumber)
    case ClaimantType.User      => userEoriNumber
  }

  def getClaimantType(userEoriNumber: Eori): ClaimantType =
    if (getConsigneeEoriFromACC14.contains(userEoriNumber))
      ClaimantType.Consignee
    else if (getDeclarantEoriFromACC14.contains(userEoriNumber))
      ClaimantType.Declarant
    else
      ClaimantType.User

  def getClaimantInformation(userEoriNumber: Eori): Option[ClaimantInformation] =
    for {
      contactDetails <- mrnContactDetailsAnswer
      contactAddress <- mrnContactAddressAnswer
    } yield ClaimantInformation.from(
      getClaimantEori(userEoriNumber),
      getClaimantType(userEoriNumber) match {
        case ClaimantType.Consignee => displayDeclaration.flatMap(_.getConsigneeDetails)
        case ClaimantType.Declarant => displayDeclaration.map(_.getDeclarantDetails)
        case ClaimantType.User      => displayDeclaration.map(_.getDeclarantDetails)
      },
      contactDetails,
      contactAddress
    )

  def computeClaimantInformation(user: SignedInUserDetails): Option[ClaimantInformation] =
    for {
      contactDetails <- computeContactDetails(user)
      contactAddress <- computeAddressDetails(user)
    } yield ClaimantInformation.from(
      getClaimantEori(user.eori),
      getClaimantType(user.eori) match {
        case ClaimantType.Consignee => displayDeclaration.flatMap(_.getConsigneeDetails)
        case ClaimantType.Declarant => displayDeclaration.map(_.getDeclarantDetails)
        case ClaimantType.User      => displayDeclaration.map(_.getDeclarantDetails)
      },
      contactDetails,
      contactAddress
    )

}

object DraftClaim {

  val blank: DraftClaim = DraftClaim(UUID.randomUUID())

  implicit val eq: Eq[DraftClaim]          = Eq.fromUniversalEquals
  implicit val format: OFormat[DraftClaim] = derived.oformat()

  class LeadAndAssociatedItems[A](leadItem: => Option[A], associatedItems: => Option[NonEmptyList[A]]) {

    lazy val list: List[A] = {
      leadItem.toList ++ associatedItems.toList.flatMap(_.toList)
    }

    lazy val nonEmptyListOpt: Option[NonEmptyList[A]] =
      leadItem.map(NonEmptyList.of(_, associatedItems.map(_.toList).getOrElse(Nil): _*))

    def get(index: Int): Option[A] =
      list.get(index.toLong)
  }

  class LeadAndAssociatedItemList[A](
    leadItem: => Option[NonEmptyList[A]],
    associatedItems: => Option[NonEmptyList[NonEmptyList[A]]]
  ) {

    lazy val list: List[List[A]] = {
      val x  = leadItem.map(_.toList).getOrElse(Nil)
      val xs = associatedItems
        .map(_.map(_.toList).toList)
        .getOrElse(Nil)
      x :: xs
    }

    lazy val nonEmptyListOpt: Option[NonEmptyList[NonEmptyList[A]]] =
      leadItem.map(NonEmptyList.of(_, associatedItems.map(_.toList).getOrElse(Nil): _*))

    def get(index: Int): Option[List[A]] =
      list.get(index.toLong)
  }
}
