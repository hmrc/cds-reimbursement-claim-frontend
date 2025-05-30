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

object JourneyValidationErrors {

  val EORI_NOT_ALLOWLISTED: String                     = "userEoriNotAllowlisted"
  val JOURNEY_ALREADY_FINALIZED: String                = "journeyAlreadyFinalized"
  val MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER: String  = "missingFirstMovementReferenceNumber"
  val MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER: String = "missingSecondMovementReferenceNumber"
  val MISSING_DISPLAY_DECLARATION: String              = "missingDisplayDeclaration"
  val UNSUPPORTED_TAX_CODES: String                    = "unsupportedTaxCodes"
  val MISSING_BASIS_OF_CLAIM: String                   = "missingBasisOfClaim"
  val MISSING_DETAILS_OF_OVERPAYMENT: String           = "missingDetailsOfOverpayment"
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
  val MISSING_EXPORT_MOVEMENT_REFERENCE_NUMBER: String = "missingExportMovementReferenceNumber"
  val MISSING_NEW_EORI: String                         = "missingNewEori"
  val MISSING_NEW_DAN: String                          = "missingNewDan"
  val MISSING_BILL_OF_DISCHARGE_3_DOCUMENTS: String    = "missingBillOfDischarge3Documents"
  val MISSING_BILL_OF_DISCHARGE_4_DOCUMENTS: String    = "missingBillOfDischarge4Documents"
  val MISSING_PROOF_OF_ORIGIN_DOCUMENTS: String        = "missingProofOfOriginDocuments"
  val INVALID_REASON_FOR_SECURITY: String              = "invalidReasonForSecurity"
  val ADDITIONAL_DETAILS_NOT_YET_VISITED: String       = "additionalDetailsNotYetVisited"

  val EXPECTED_SINGLE_EXPORT_MOVEMENT_REFERENCE_NUMBER: String    = "expectedSingleExportMovementReferenceNumber"
  val EXPECTED_MULTIPLE_EXPORT_MOVEMENT_REFERENCE_NUMBERS: String = "expectedMultipleExportMovementReferenceNumbers"

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
  val PAYEE_TYPE_MUST_BE_DEFINED: String                                        =
    "payeeTypeMustBeDefined"
  val PAYEE_TYPE_MUST_BE_NOT_DEFINED: String                                    =
    "payeeTypeMustBeNotDefined"
  val BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED: String                              =
    "bankAccountDetailsMustBeDefined"
  val BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED: String                          =
    "bankAccountDetailsMustNotBeDefined"
  val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED: String              =
    "basisOfClaimSpecialCircumstancesMustBeDefined when basisOfClaim value is SpecialCircumstances"
  val BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED: String          =
    "basisOfClaimSpecialCircumstancesMustNotBeDefined when basisOfClaim value is not SpecialCircumstances"
  val REIMBURSEMENT_METHOD_MUST_BE_DEFINED: String                              =
    "reimbursementMethodMustBeDefined when all selected duties are CMA eligible"
  val REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED: String                   =
    "ReimbursementMethodMustNotBeDefined when not all of selected duties are CMA eligible"
  val CHOOSEN_REASON_FOR_SECURITY_REQUIRES_GOODS_TO_BE_ALREADY_EXPORTED: String =
    "choosenReasonForSecurityRequiresGoodsToBeAlreadyExported"
  val MISSING_CLAIM_DUPLICATE_CHECK_STATUS_WITH_TPI04: String                   =
    "missingClaimDuplicateCheckStatusWithTPI04"
  val SIMILAR_CLAIM_EXISTS_ALREADY_IN_CDFPAY: String                            =
    "similarClaimExistAlreadyInCDFPay"
  val DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED: String               =
    "duplicateMovementReferenceNumber when basisOfClaim value is DuplicateEntry"
  val DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_NOT_BE_DEFINED: String           =
    "duplicateMovementReferenceNumber when basisOfClaim value is not DuplicateEntry"
  val DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED: String                     =
    "duplicateDisplayDeclaration must be defined when basisOfClaim value is DuplicateEntry"
  val DUPLICATE_DISPLAY_DECLARATION_MUST_BE_VERIFIED: String                    =
    "duplicateDisplayDeclaration must be verified"
  val DUPLICATE_DISPLAY_DECLARATION_MUST_NOT_BE_DEFINED: String                 =
    "duplicateDisplayDeclaration must not be defined when basisOfClaim value is not DuplicateEntry"
  val DUPLICATE_MOVEMENT_REFERENCE_NUMBER_NOT_REQUIRED: String                  =
    "duplicateMovementReferenceNumberNotRequired"
  val DUTIES_CHANGE_MODE_ENABLED: String                                        =
    "dutiesChangeModeEnabled"
  val ENTER_CONTACT_DETAILS_MODE_ENABLED: String                                =
    "enterContactDetailsModeEnabled"
  val DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT: String                           =
    "displayDeclarationHasSubsidyPayment"
  val DISPLAY_DECLARATION_MUST_HAVE_ONLY_SUBSIDY_PAYMENTS: String               =
    "displayDeclarationMustHaveOnlySubsidyPayments"
  val SHOULD_MATCH_ACC14_CONSIGNEE_EORI: String                                 =
    "submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14"
  val SHOULD_MATCH_ACC14_DUPLICATE_CONSIGNEE_EORI                               =
    "checkConsigneeEoriNumberWithDuplicateDeclaration.shouldMatchConsigneeEoriFromACC14"
  val SHOULD_MATCH_ACC14_DECLARANT_EORI: String                                 =
    "submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14"
  val SHOULD_MATCH_ACC14_DUPLICATE_DECLARANT_EORI: String                       =
    "submitDuplicateDeclarantEoriNumber.shouldMatchDuplicateDeclarantEoriFromACC14"

}
