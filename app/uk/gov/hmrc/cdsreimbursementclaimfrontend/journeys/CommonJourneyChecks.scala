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

import JourneyValidationErrors._
import com.github.arturopala.validator.Validator._

trait CommonJourneyChecks[Journey <: CommonJourneyProperties] {

  final val hasMovementReferenceNumber: Validate[Journey] =
    checkIsTrue(
      journey => journey.getLeadMovementReferenceNumber.isDefined,
      MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER
    )

  final val hasDisplayDeclaration: Validate[Journey] =
    checkIsTrue(
      journey => journey.getLeadDisplayDeclaration.isDefined,
      MISSING_DISPLAY_DECLARATION
    )

  final val hasMRNAndDisplayDeclaration: Validate[Journey] =
    hasMovementReferenceNumber & hasDisplayDeclaration

  final val declarantOrImporterEoriMatchesUserOrHasBeenVerified: Validate[Journey] =
    conditionally[Journey](
      _.needsDeclarantAndConsigneeEoriSubmission,
      all(
        checkIsDefined(
          _.answers.declarantEoriNumber,
          DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED
        ),
        checkEquals(
          _.getDeclarantEoriFromACC14,
          _.answers.declarantEoriNumber,
          DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14
        ),
        checkIsDefined(
          _.answers.consigneeEoriNumber,
          CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED
        ),
        checkEquals(
          _.getConsigneeEoriFromACC14,
          _.answers.consigneeEoriNumber,
          CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14
        )
      ),
      all(
        checkIsEmpty(
          _.answers.declarantEoriNumber,
          DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED
        ),
        checkIsEmpty(
          _.answers.consigneeEoriNumber,
          CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED
        )
      )
    )

  final val paymentMethodHasBeenProvidedIfNeeded: Validate[Journey] =
    conditionally[Journey](
      _.needsBanksAccountDetailsSubmission,
      checkIsDefined(
        _.answers.bankAccountDetails,
        BANK_ACCOUNT_DETAILS_MUST_BE_DEFINED
      ),
      checkIsEmpty(
        _.answers.bankAccountDetails,
        BANK_ACCOUNT_DETAILS_MUST_NOT_BE_DEFINED
      )
    )

  final val contactDetailsHasBeenProvided: Validate[Journey] =
    checkIsDefined[Journey](_.answers.contactDetails, MISSING_CONTACT_DETAILS) &
      checkIsDefined[Journey](_.answers.contactAddress, MISSING_CONTACT_ADDRESS)

  final val supportingEvidenceHasBeenProvided: Validate[Journey] =
    checkIsTrue(_.hasCompleteSupportingEvidences, INCOMPLETE_SUPPORTING_EVIDENCES)

}
