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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import cats.syntax.eq._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

/** Common properties and computations of all of the journeys. */
trait CommonJourneyProperties {

  def answers: CommonAnswers

  /** Case numer is the final result of successfully submitting the claim. */
  def caseNumber: Option[String]

  def getLeadMovementReferenceNumber: Option[MRN]
  def getLeadDisplayDeclaration: Option[DisplayDeclaration]
  def needsBanksAccountDetailsSubmission: Boolean

  final val ZERO: BigDecimal = BigDecimal("0")

  final def hasCompleteSupportingEvidences: Boolean =
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
    !(getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromACC14.contains(answers.userEoriNumber))

  final def needsDeclarantAndConsigneePostCode: Boolean =
    !isConsigneePostCodeFromAcc14.getOrElse(false) && !isDeclarantPostCodeFromAcc14.getOrElse(false)

  final def getConsigneeBankAccountDetails: Option[BankAccountDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.displayResponseDetail.bankDetails.flatMap(_.consigneeBankDetails))

  final def getDeclarantBankAccountDetails: Option[BankAccountDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.displayResponseDetail.bankDetails.flatMap(_.declarantBankDetails))

  final def computeBankAccountDetails: Option[BankAccountDetails] =
    answers.bankAccountDetails
      .orElse(getConsigneeBankAccountDetails)
      .orElse(getDeclarantBankAccountDetails)

  final def needsProofOfAuthorityForBankAccountDetailsChange: Boolean =
    answers.bankAccountDetails.exists { bankDetails =>
      getConsigneeBankAccountDetails.forall(_ =!= bankDetails) &&
      getDeclarantBankAccountDetails.forall(_ =!= bankDetails)
    }

  final def getClaimantType: ClaimantType =
    if (getConsigneeEoriFromACC14.contains(answers.userEoriNumber))
      ClaimantType.Consignee
    else if (getDeclarantEoriFromACC14.contains(answers.userEoriNumber))
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

  final def computeContactDetails(retrievedUser: RetrievedUserType): Option[MrnContactDetails] = (
    answers.contactDetails,
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)),
    getLeadDisplayDeclaration.flatMap(_.getDeclarantDetails.contactDetails),
    retrievedUser
  ) match {
    case (details @ Some(_), _, _, _)                                                                              =>
      details
    case (_, Some(consigneeContactDetails), _, user) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(
        MrnContactDetails(
          consigneeContactDetails.contactName.getOrElse(""),
          consigneeContactDetails.emailAddress
            .fold(user.email.getOrElse(Email("")))(address => Email(address)),
          consigneeContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case (_, None, _, user) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber)                          =>
      Some(
        MrnContactDetails(
          user.name.map(_.toFullName).getOrElse(""),
          user.email.getOrElse(Email("")),
          None
        )
      )
    case (_, _, Some(declarantContactDetails), user)                                                               =>
      Some(
        MrnContactDetails(
          declarantContactDetails.contactName.getOrElse(""),
          declarantContactDetails.emailAddress
            .fold(user.email.getOrElse(Email("")))(address => Email(address)),
          declarantContactDetails.telephone.map(PhoneNumber(_))
        )
      )
    case _                                                                                                         => None
  }

  final def computeAddressDetails: Option[ContactAddress] = (
    answers.contactAddress,
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails),
    getLeadDisplayDeclaration.map(_.getDeclarantDetails)
  ) match {
    case (contactAddress @ Some(_), _, _)                                                                =>
      contactAddress
    case (None, Some(consigneeDetails), _) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(consigneeDetails.establishmentAddress.toContactAddress)
    case (None, _, Some(declarantDetails))                                                               =>
      Some(declarantDetails.establishmentAddress.toContactAddress)
    case _                                                                                               => None
  }

}
