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

import com.github.arturopala.validator.Validator.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

trait CommonJourneyChecks[J <: CommonJourneyProperties] {

  final val hasMovementReferenceNumber: Validate[J] =
    checkIsTrue(
      journey => journey.getLeadMovementReferenceNumber.isDefined,
      MISSING_FIRST_MOVEMENT_REFERENCE_NUMBER
    )

  final val hasDisplayDeclaration: Validate[J] =
    checkIsTrue(
      journey => journey.getLeadDisplayDeclaration.isDefined,
      MISSING_DISPLAY_DECLARATION
    )

  final val hasMRNAndDisplayDeclaration: Validate[J] =
    hasMovementReferenceNumber & hasDisplayDeclaration

  final val containsOnlySupportedTaxCodes: Validate[J] =
    checkIsTrue(
      journey =>
        journey.getDisplayDeclarations
          .forall(_.containsOnlySupportedTaxCodes),
      UNSUPPORTED_TAX_CODES
    )

  final val declarantOrImporterEoriMatchesUserOrHasBeenVerified: Validate[J] =
    conditionally[J](
      _.needsDeclarantAndConsigneeEoriSubmission,
      all(
        checkIsDefined(
          _.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber),
          DECLARANT_EORI_NUMBER_MUST_BE_PROVIDED
        ),
        checkEquals(
          _.getDeclarantEoriFromACC14,
          _.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber),
          DECLARANT_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14
        ),
        whenTrue(
          _.getConsigneeEoriFromACC14.isDefined,
          all(
            checkIsDefined(
              _.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber),
              CONSIGNEE_EORI_NUMBER_MUST_BE_PROVIDED
            ),
            checkEquals(
              _.getConsigneeEoriFromACC14,
              _.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber),
              CONSIGNEE_EORI_NUMBER_MUST_BE_EQUAL_TO_THAT_OF_ACC14
            )
          )
        )
      ),
      all(
        checkIsEmpty(
          _.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber),
          DECLARANT_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED
        ),
        checkIsEmpty(
          _.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber),
          CONSIGNEE_EORI_NUMBER_DOES_NOT_HAVE_TO_BE_PROVIDED
        )
      )
    )

  final val payeeTypeIsDefined: Validate[J] =
    checkIsDefined(
      _.answers.payeeType,
      PAYEE_TYPE_MUST_BE_DEFINED
    )

  final val paymentMethodHasBeenProvidedIfNeeded: Validate[J] =
    conditionally[J](
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

  final val contactDetailsHasBeenProvided: Validate[J] =
    checkIsDefined[J](_.answers.contactDetails, MISSING_CONTACT_DETAILS) &
      checkIsDefined[J](_.answers.contactAddress, MISSING_CONTACT_ADDRESS)

  final val supportingEvidenceHasBeenProvided: Validate[J] =
    checkIsTrue(_.hasCompleteSupportingEvidences, INCOMPLETE_SUPPORTING_EVIDENCES)

  final def declarationHasNoSubsidyPayments: Validate[DisplayDeclaration] =
    checkIsFalse(_.hasSomeSubsidyPayment, DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

  final val declarationsHasNoSubsidyPayments: Validate[J] =
    checkEach(_.getDisplayDeclarations, declarationHasNoSubsidyPayments)

  final def declarationHasOnlySubsidyPayments: Validate[DisplayDeclaration] =
    checkIsTrue(_.hasOnlySubsidyPayments, DISPLAY_DECLARATION_MUST_HAVE_ONLY_SUBSIDY_PAYMENTS)

  final val declarationsHasOnlySubsidyPayments: Validate[J] =
    checkEach(_.getDisplayDeclarations, declarationHasOnlySubsidyPayments)

}
