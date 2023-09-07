/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import cats.syntax.eq._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AuthenticatedUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails

/** Common properties and computations of all of the journeys. */
trait CommonJourneyProperties {

  def answers: CommonAnswers

  /** Case number is the final result of successfully submitting the claim. */
  def caseNumber: Option[String]

  def getLeadMovementReferenceNumber: Option[MRN]
  def getLeadDisplayDeclaration: Option[DisplayDeclaration]
  def needsBanksAccountDetailsSubmission: Boolean
  def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]]

  def getDisplayDeclarations: Seq[DisplayDeclaration] =
    getLeadDisplayDeclaration.fold(Seq.empty[DisplayDeclaration])(Seq.apply(_))

  def validateDeclarationCandidate(declaration: DisplayDeclaration): Option[String] =
    None

  final val ZERO: BigDecimal = BigDecimal("0")

  final def hasCompleteSupportingEvidences: Boolean =
    answers.supportingEvidences.nonEmpty &&
      answers.supportingEvidences.forall(_.documentType.isDefined)

  final def getConsigneeEoriFromACC14: Option[Eori] =
    getLeadDisplayDeclaration.flatMap(_.getConsigneeEori)

  final def getConsigneeContactDetailsFromACC14: Option[ContactDetails] =
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails).flatMap(_.contactDetails)

  final def getDeclarantEoriFromACC14: Option[Eori] =
    getLeadDisplayDeclaration.map(_.getDeclarantEori)

  final def getDeclarantContactDetailsFromACC14: Option[ContactDetails] =
    getLeadDisplayDeclaration.flatMap(_.getDeclarantDetails.contactDetails)

  final def isConsigneePostCodeFromAcc14: Option[Boolean] =
    getLeadDisplayDeclaration.map(
      _.getConsigneeDetails.exists(_.establishmentAddress.postalCode.isEmpty)
    )

  final def isDeclarantPostCodeFromAcc14: Option[Boolean] =
    getLeadDisplayDeclaration.map(_.getDeclarantDetails).map(_.establishmentAddress.postalCode.isEmpty)

  /** Check if ACC14 have declarant EORI or consignee EORI matching user's EORI */
  final def needsDeclarantAndConsigneeEoriSubmission: Boolean =
    !(userHasGBEoriMatchingDeclaration || userHasXIEoriMatchingDeclaration)

  final def needsUserXiEoriSubmission: Boolean =
    !userHasGBEoriMatchingDeclaration &&
      getLeadDisplayDeclaration.exists(_.containsXiEori) &&
      answers.eoriNumbersVerification.flatMap(_.userXiEori).isEmpty

  final def userHasGBEoriMatchingDeclaration: Boolean =
    getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromACC14.contains(answers.userEoriNumber)

  final def userHasXIEoriMatchingDeclaration: Boolean =
    answers.eoriNumbersVerification.exists(x =>
      x.hasSameXiEoriAs(getDeclarantEoriFromACC14) ||
        x.hasSameXiEoriAs(getConsigneeEoriFromACC14)
    )

  final def needsDeclarantAndConsigneePostCode: Boolean =
    !isConsigneePostCodeFromAcc14.getOrElse(false) &&
      !isDeclarantPostCodeFromAcc14.getOrElse(false)

  final def getConsigneeBankAccountDetails: Option[BankAccountDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.displayResponseDetail.bankDetails.flatMap(_.consigneeBankDetails))

  final def getDeclarantBankAccountDetails: Option[BankAccountDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.displayResponseDetail.bankDetails.flatMap(_.declarantBankDetails))

  final def computeBankAccountDetails: Option[BankAccountDetails] =
    answers.bankAccountDetails
      .orElse(getInitialBankAccountDetailsFromDeclaration)

  final def getInitialBankAccountDetailsFromDeclaration: Option[BankAccountDetails] =
    getConsigneeBankAccountDetails.orElse(getDeclarantBankAccountDetails)

  final def needsProofOfAuthorityForBankAccountDetailsChange: Boolean =
    answers.bankAccountDetails.exists { bankDetails =>
      getConsigneeBankAccountDetails.forall(_ =!= bankDetails) &&
      getDeclarantBankAccountDetails.forall(_ =!= bankDetails)
    }

  final def haveBankDetailsOnAcc14: Boolean =
    getLeadDisplayDeclaration
      .exists(_.hasBankDetails)

  final def getClaimantType: ClaimantType =
    if (
      getConsigneeEoriFromACC14.exists(eori =>
        answers.userEoriNumber === eori ||
          answers.eoriNumbersVerification.exists(_.hasSameXiEoriAs(eori))
      )
    )
      ClaimantType.Consignee
    else if (
      getDeclarantEoriFromACC14.exists(eori =>
        answers.userEoriNumber === eori ||
          answers.eoriNumbersVerification.exists(_.hasSameXiEoriAs(eori))
      )
    )
      ClaimantType.Declarant
    else
      ClaimantType.User

  final def getClaimantEori: Eori = getClaimantType match {
    case ClaimantType.Consignee => getConsigneeEoriFromACC14.getOrElse(answers.userEoriNumber)
    case ClaimantType.Declarant => getDeclarantEoriFromACC14.getOrElse(answers.userEoriNumber)
    case ClaimantType.User      => answers.userEoriNumber
  }

  final def getClaimantInformation: Option[ClaimantInformation] =
    for {
      contactDetails <- answers.contactDetails
      contactAddress <- answers.contactAddress
    } yield ClaimantInformation.from(
      getClaimantEori,
      getClaimantType match {
        case ClaimantType.Consignee => getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails)
        case ClaimantType.Declarant => getLeadDisplayDeclaration.map(_.getDeclarantDetails)
        case ClaimantType.User      => getLeadDisplayDeclaration.map(_.getDeclarantDetails)
      },
      contactDetails,
      contactAddress
    )

  final def computeContactDetails(
    authenticatedUser: AuthenticatedUser,
    verifiedEmailOpt: Option[CdsVerifiedEmail]
  ): Option[MrnContactDetails] =
    Some(
      answers.contactDetails.getOrElse(
        getInitialContactDetailsFromDeclarationAndCurrentUser(authenticatedUser, verifiedEmailOpt)
      )
    )

  final def getInitialContactDetailsFromDeclarationAndCurrentUser(
    authenticatedUser: AuthenticatedUser,
    verifiedEmailOpt: Option[CdsVerifiedEmail]
  ): MrnContactDetails = {
    def currentUserEmail = verifiedEmailOpt
      .map(_.toEmail)
      .orElse(authenticatedUser.email)
    (
      getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)),
      getLeadDisplayDeclaration.flatMap(_.getDeclarantDetails.contactDetails)
    ) match {
      case (Some(consigneeContactDetails), _) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
        MrnContactDetails(
          consigneeContactDetails.contactName.getOrElse(""),
          consigneeContactDetails.maybeEmailAddress
            .fold(currentUserEmail)(address => Some(Email(address))),
          consigneeContactDetails.telephone.map(PhoneNumber(_))
        )

      case (_, Some(declarantContactDetails)) if getDeclarantEoriFromACC14.contains(answers.userEoriNumber) =>
        MrnContactDetails(
          declarantContactDetails.contactName.getOrElse(""),
          declarantContactDetails.maybeEmailAddress
            .fold(currentUserEmail)(address => Some(Email(address))),
          declarantContactDetails.telephone.map(PhoneNumber(_))
        )

      case _ =>
        MrnContactDetails(
          authenticatedUser.name.map(_.toFullName).getOrElse(""),
          currentUserEmail,
          None
        )
    }
  }

  final def emailAddressHasChanged: Boolean =
    answers.contactDetails.exists(_.emailAddressHasChanged)

  final def contactNameHasChanged: Boolean =
    answers.contactDetails.exists(_.nameHasChanged)

  final def phoneNumberHasChanged: Boolean =
    answers.contactDetails.exists(_.phoneNumberHasChanged)

  final def contactAddressHasChanged: Boolean =
    answers.contactAddress.exists(_.addressHasChanged)

  final def bankAccountHasChanged: Boolean =
    answers.bankAccountDetails.exists(_.bankAccountHasChanged)

  final def computeAddressDetails: Option[ContactAddress] =
    answers.contactAddress.orElse(getInitialAddressDetailsFromDeclaration)

  final def getInitialAddressDetailsFromDeclaration: Option[ContactAddress] = (
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails),
    getLeadDisplayDeclaration.map(_.getDeclarantDetails)
  ) match {
    case (Some(consigneeDetails), _) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(consigneeDetails.establishmentAddress.toContactAddress)
    case (_, Some(declarantDetails))                                                               =>
      Some(declarantDetails.establishmentAddress.toContactAddress)
    case _                                                                                         => None
  }

  protected def nextAfter[A](item: A)(seq: Seq[A]): Option[A] = {
    val i = seq.indexOf(item)
    if (i === -1) None
    else if (i === seq.size - 1) None
    else Some(seq(i + 1))
  }

  final def declarationsHasOnlySubsidyPayments: Boolean =
    getDisplayDeclarations.nonEmpty &&
      getDisplayDeclarations.forall(_.hasOnlySubsidyPayments)

}
