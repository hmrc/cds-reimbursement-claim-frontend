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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email

object RejectedGoods {

  /** Common answers of the rejected-goods single, multiple and scheduled journeys. */
  trait CommonAnswers {
    def nonce: Nonce
    def userEoriNumber: Eori
    def consigneeEoriNumber: Option[Eori]
    def declarantEoriNumber: Option[Eori]
    def contactDetails: Option[MrnContactDetails]
    def contactAddress: Option[ContactAddress]
    def basisOfClaim: Option[BasisOfRejectedGoodsClaim]
    def basisOfClaimSpecialCircumstances: Option[String]
    def methodOfDisposal: Option[MethodOfDisposal]
    def detailsOfRejectedGoods: Option[String]
    def inspectionDate: Option[InspectionDate]
    def inspectionAddress: Option[InspectionAddress]
    def bankAccountDetails: Option[BankAccountDetails]
    def bankAccountType: Option[BankAccountType]
    def selectedDocumentType: Option[UploadDocumentType]
    def supportingEvidences: Seq[UploadedFile]
    def checkYourAnswersChangeMode: Boolean
  }

  /** Common properties of the rejected-goods single, multiple and scheduled journeys. */
  trait CommonJourneyProperties {

    def answers: CommonAnswers
    def getLeadDisplayDeclaration: Option[DisplayDeclaration]

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

    def getPotentialInspectionAddresses: Seq[(InspectionAddressType, String)] =
      Seq(
        getConsigneeContactDetailsFromACC14.flatMap(_.showAddress).map(Importer  -> _),
        getDeclarantContactDetailsFromACC14.flatMap(_.showAddress).map(Declarant -> _)
      ).flatten(Option.option2Iterable)

    def getInspectionAddressForType(
      addressType: InspectionAddressType
    ): Option[InspectionAddress] =
      addressType match {
        case Importer  => getConsigneeContactDetailsFromACC14.map(InspectionAddress.ofType(addressType).mapFrom(_))
        case Declarant => getDeclarantContactDetailsFromACC14.map(InspectionAddress.ofType(addressType).mapFrom(_))
        case Other     => None
      }

    final def isDeclarantPostCodeFromAcc14: Option[Boolean] =
      getLeadDisplayDeclaration.map(_.getDeclarantDetails).map(_.establishmentAddress.postalCode.isEmpty)

    /** Check if ACC14 have declarant EORI or consignee EORI matching user's EORI */
    final def needsDeclarantAndConsigneeEoriSubmission: Boolean =
      !(getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
        getConsigneeEoriFromACC14.contains(answers.userEoriNumber))

    final def needsDeclarantAndConsigneePostCode: Boolean =
      !isConsigneePostCodeFromAcc14.getOrElse(false) && !isDeclarantPostCodeFromAcc14.getOrElse(false)

    final def needsSpecialCircumstancesBasisOfClaim: Boolean =
      answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances)

    final def computeBankAccountDetails: Option[BankAccountDetails] =
      Stream(
        answers.bankAccountDetails,
        getLeadDisplayDeclaration.flatMap(_.displayResponseDetail.bankDetails.flatMap(_.consigneeBankDetails)),
        getLeadDisplayDeclaration.flatMap(_.displayResponseDetail.bankDetails.flatMap(_.declarantBankDetails))
      ).find(_.nonEmpty).flatten

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

    final def getInspectionAddressType: Option[InspectionAddressType] =
      answers.inspectionAddress.map(_.addressType)

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

}
