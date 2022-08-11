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

object JourneyValidationErrors {

  val JOURNEY_ALREADY_FINALIZED: String                = "journeyAlreadyFinalized"
  val MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER: String  = "missingFirstMovementReferenceNumber"
  val MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER: String = "missingSecondMovementReferenceNumber"
  val MISSING_DISPLAY_DECLARATION: String              = "missingDisplayDeclaration"
  val MISSING_BASIS_OF_CLAIM: String                   = "missingBasisOfClaim"
  val MISSING_DETAILS_OF_REJECTED_GOODS: String        = "missingDetailsOfRejectedGoods"
  val MISSING_INSPECTION_DATE: String                  = "missingInspectionDate"
  val MISSING_INSPECTION_ADDRESS: String               = "missingInspectionAddress"
  val MISSING_METHOD_OF_DISPOSAL: String               = "missingMethodOfDisposal"
  val INCOMPLETE_REIMBURSEMENT_CLAIMS: String          = "incompleteReimbursementClaims"
  val INCOMPLETE_SECURITIES_RECLAIMS: String           = "incompleteSecuritiesReclaims"
  val INCOMPLETE_SUPPORTING_EVIDENCES: String          = "incompleteSupportingEvidences"
  val MISSING_CONTACT_DETAILS: String                  = "missingContactDetails"
  val MISSING_CONTACT_ADDRESS: String                  = "missingContactAddress"
  val MISSING_SCHEDULED_DOCUMENT: String               = "missingScheduledDocument"
  val MISSING_REASON_FOR_SECURITY: String              = "missingReasonForSecurity"
  val MISSING_EXPORT_MOVEMENT_REFERENCE_NUMBER         = "missingExportMovementReferenceNumber"

  val TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO: String              = "totalReimbursementAmountMustBeGreaterThanZero"
  val DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED: String                            =
    "declarantEoriNumberMustBeProvided if user's EORI is not matching those of ACC14 declarant or consignee"
  val DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14: String              = "declarantEoriNumberMustBeEqualToThatOfACC14"
  val CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED: String                            =
    "consigneeEoriNumberMustBeProvided if user's EORI is not matching those of ACC14 declarant or consignee"
  val CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14: String              = "consigneeEoriNumberMustBeEqualToThatOfACC14"
  val DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED: String                =
    "declarantEoriNumberDoesNotHaveToBeProvided if user's EORI is matching those of ACC14 declarant or consignee"
  val CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED: String                =
    "consigneeEoriNumberDoesNotHaveToBeProvided if user's EORI is matching those of ACC14 declarant or consignee"
  val BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED: String                              =
    "bankAccountDetailsMustBeDefined when reimbursementMethodAnswer is empty or not CurrentMonthAdjustment"
  val BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED: String                          =
    "bankAccountDetailsMustNotBeDefined when reimbursementMethodAnswer is CurrentMonthAdjustment"
  val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED: String              =
    "basisOfClaimSpecialCircumstancesMustBeDefined when basisOfClaim value is SpecialCircumstances"
  val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED: String          =
    "basisOfClaimSpecialCircumstancesMustNotBeDefined when basisOfClaim value is not SpecialCircumstances"
  val REIMBURSEMENT_METHOD_MUST_BE_DEFINED: String                              =
    "reimbursementMethodMustBeDefined when all selected duties are CMA eligible"
  val REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED: String                   =
    "reimbursementMethodAnswerMustNotBeDefined when not all of selected duties are CMA eligible"
  val CHOOSEN_REASON_FOR_SECURITY_REQUIRES_GOODS_TO_BE_ALREADY_EXPORTED: String =
    "choosenReasonForSecurityRequiresGoodsToBeAlreadyExported"
  val MISSING_CLAIM_DUPLICATE_CHECK_STATUS_WITH_TPI04: String                   =
    "missingClaimDuplicateCheckStatusWithTPI04"
  val SIMILAR_CLAIM_EXISTS_ALREADY_IN_CDFPAY: String                            =
    "similarClaimExistAlreadyInCDFPay"
}
