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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoods.ValidationErrors._

trait RejectedGoodsMultipleJourneyRouter {

  private val undefined: Call = routes.WorkInProgressController.show()

  def routeForValidationError(error: String): Call =
    error match {
      case JOURNEY_ALREADY_FINALIZED                                => routes.CheckYourAnswersController.showConfirmation()
      case MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER                  => undefined //routes.EnterMovementReferenceNumberController.show()
      case MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER                 => undefined //routes.EnterMovementReferenceNumberController.show()
      case MISSING_DISPLAY_DECLARATION                              => undefined //routes.EnterMovementReferenceNumberController.show()
      case MISSING_BASIS_OF_CLAIM                                   => routes.BasisForClaimController.show()
      case MISSING_DETAILS_OF_REJECTED_GOODS                        => routes.EnterRejectedGoodsDetailsController.show()
      case MISSING_INSPECTION_DATE                                  => routes.EnterInspectionDateController.show()
      case MISSING_INSPECTION_ADDRESS                               => routes.ChooseInspectionAddressTypeController.show()
      case MISSING_METHOD_OF_DISPOSAL                               => routes.DisposalMethodController.show()
      case INCOMPLETE_REIMBURSEMENT_CLAIMS                          => undefined
      case INCOMPLETE_SUPPORTING_EVIDENCES                          => undefined //routes.ChooseFileTypeController.show()
      case MISSING_CONTACT_DETAILS                                  => routes.EnterContactDetailsController.show()
      case MISSING_CONTACT_ADDRESS                                  => routes.CheckClaimantDetailsController.redirectToALF()
      case TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO     => undefined
      case DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED                   => undefined //routes.EnterDeclarantEoriNumberController.show()
      case DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14     =>
        undefined //routes.EnterDeclarantEoriNumberController.show()
      case CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED                   => undefined //routes.EnterImporterEoriNumberController.show()
      case CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14     =>
        undefined //routes.EnterImporterEoriNumberController.show()
      case DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED       => undefined
      case CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED       => undefined
      case BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED                     => routes.CheckBankDetailsController.show()
      case BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED                 => undefined
      case BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED     => routes.EnterSpecialCircumstancesController.show()
      case BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED => undefined
      case REIMBURSEMENT_METHOD_MUST_BE_DEFINED                     => undefined
      case REIMBURSEMENT_METHOD_ANSWER_MUST_NOT_BE_DEFINED          => undefined
      case _                                                        => undefined
    }

  def routeForValidationErrors(errors: Seq[String]): Call =
    errors.headOption.map(routeForValidationError).getOrElse(undefined)

}
