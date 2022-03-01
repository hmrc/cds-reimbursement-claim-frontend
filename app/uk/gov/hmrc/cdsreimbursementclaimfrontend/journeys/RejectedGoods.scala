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

  /** Final journey validation errors. */
  object ValidationErrors {
    val JOURNEY_ALREADY_FINALIZED: String                                = "journeyAlreadyFinalized"
    val MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER: String                  = "missingFirstMovementReferenceNumber"
    val MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER: String                 = "missingSecondMovementReferenceNumber"
    val MISSING_DISPLAY_DECLARATION: String                              = "missingDisplayDeclaration"
    val MISSING_BASIS_OF_CLAIM: String                                   = "missingBasisOfClaim"
    val MISSING_DETAILS_OF_REJECTED_GOODS: String                        = "missingDetailsOfRejectedGoods"
    val MISSING_INSPECTION_DATE: String                                  = "missingInspectionDate"
    val MISSING_INSPECTION_ADDRESS: String                               = "missingInspectionAddress"
    val MISSING_METHOD_OF_DISPOSAL: String                               = "missingMethodOfDisposal"
    val INCOMPLETE_REIMBURSEMENT_CLAIMS: String                          = "incompleteReimbursementClaims"
    val INCOMPLETE_SUPPORTING_EVIDENCES: String                          = "incompleteSupportingEvidences"
    val MISSING_CONTACT_DETAILS: String                                  = "missingContactDetails"
    val MISSING_CONTACT_ADDRESS: String                                  = "missingContactAddress"
    val TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO: String     = "totalReimbursementAmountMustBeGreaterThanZero"
    val DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED: String                   =
      "declarantEoriNumberMustBeProvided if user's EORI is not matching those of ACC14 declarant or consignee"
    val DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14: String     = "declarantEoriNumberMustBeEqualToThatOfACC14"
    val CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED: String                   =
      "consigneeEoriNumberMustBeProvided if user's EORI is not matching those of ACC14 declarant or consignee"
    val CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14: String     = "consigneeEoriNumberMustBeEqualToThatOfACC14"
    val DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED: String       =
      "declarantEoriNumberDoesNotHaveToBeProvided if user's EORI is matching those of ACC14 declarant or consignee"
    val CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED: String       =
      "consigneeEoriNumberDoesNotHaveToBeProvided if user's EORI is matching those of ACC14 declarant or consignee"
    val BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED: String                     =
      "bankAccountDetailsMustBeDefined when reimbursementMethodAnswer is empty or not CurrentMonthAdjustment"
    val BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED: String                 =
      "bankAccountDetailsMustNotBeDefined when reimbursementMethodAnswer is CurrentMonthAdjustment"
    val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED: String     =
      "basisOfClaimSpecialCircumstancesMustBeDefined when basisOfClaim value is SpecialCircumstances"
    val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED: String =
      "basisOfClaimSpecialCircumstancesMustNotBeDefined when basisOfClaim value is not SpecialCircumstances"
    val REIMBURSEMENT_METHOD_MUST_BE_DEFINED: String                     =
      "reimbursementMethodMustBeDefined when all selected duties are CMA eligible"
    val REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED: String          =
      "reimbursementMethodAnswerMustNotBeDefined when not all of selected duties are CMA eligible"
  }

}
