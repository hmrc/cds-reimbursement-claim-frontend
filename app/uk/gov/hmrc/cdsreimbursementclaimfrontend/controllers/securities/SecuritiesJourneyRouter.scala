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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors._

trait SecuritiesJourneyRouter {

  private val undefined: Call = routes.EnterMovementReferenceNumberController.show()

  def routeForValidationError(error: String): Call =
    error match {
      case JOURNEY_ALREADY_FINALIZED                                         => routes.CheckYourAnswersController.showConfirmation()
      case MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER                           => routes.EnterMovementReferenceNumberController.show()
      case MISSING_REASON_FOR_SECURITY                                       => routes.ChooseReasonForSecurityController.show()
      case MISSING_DISPLAY_DECLARATION                                       => routes.ChooseReasonForSecurityController.show()
      case MISSING_CLAIM_DUPLICATE_CHECK_STATUS_WITH_TPI04                   => routes.ChooseReasonForSecurityController.show()
      case CHOOSEN_REASON_FOR_SECURITY_REQUIRES_GOODS_TO_BE_ALREADY_EXPORTED =>
        routes.ChooseReasonForSecurityController.show()
      case SIMILAR_CLAIM_EXISTS_ALREADY_IN_CDFPAY                            => routes.EnterMovementReferenceNumberController.show()
      case INCOMPLETE_SECURITIES_RECLAIMS                                    => routes.CheckClaimDetailsController.show()
      case INCOMPLETE_SUPPORTING_EVIDENCES                                   => routes.ChooseFileTypeController.show()
      case MISSING_CONTACT_DETAILS                                           => routes.EnterContactDetailsController.show()
      case MISSING_CONTACT_ADDRESS                                           => routes.CheckClaimantDetailsController.redirectToALF()
      case TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO              => routes.CheckClaimDetailsController.show()
      case DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED                            => routes.EnterDeclarantEoriNumberController.show()
      case DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14              => routes.EnterDeclarantEoriNumberController.show()
      case CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED                            => routes.EnterImporterEoriNumberController.show()
      case CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14              => routes.EnterImporterEoriNumberController.show()
      case DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED                => undefined
      case CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED                => undefined
      case BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED                              => routes.CheckBankDetailsController.show()
      case BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED                          => undefined
      case MISSING_EXPORT_MOVEMENT_REFERENCE_NUMBER                          =>
        routes.EnterExportMovementReferenceNumberMultipleController.show()
      case MISSING_METHOD_OF_DISPOSAL                                        => undefined
      case _                                                                 =>
        undefined
    }

  final def routeForValidationErrors(errors: Seq[String]): Call =
    errors.headOption.map(routeForValidationError).getOrElse(undefined)

}
