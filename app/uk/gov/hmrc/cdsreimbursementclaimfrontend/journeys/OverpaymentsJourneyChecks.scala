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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan

trait OverpaymentsJourneyChecks[J <: OverpaymentsJourneyProperties]
    extends CommonJourneyChecks[OverpaymentsJourneyProperties] {

  final val basisOfClaimHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.basisOfClaim, MISSING_BASIS_OF_CLAIM)

  final val additionalDetailsHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.additionalDetails, MISSING_DETAILS_OF_OVERPAYMENT)

  final val reimbursementClaimsHasBeenProvided: Validate[J] =
    all(
      checkIsTrue(_.hasCompleteReimbursementClaims, INCOMPLETE_REIMBURSEMENT_CLAIMS),
      checkIsTrue(_.getTotalReimbursementAmount > 0, TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO)
    )

  final def whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments: Validate[J] =
    whenTrue(
      journey => journey.features.exists(_.shouldBlockSubsidies) && !journey.isSubsidyOnlyJourney,
      declarationsHasNoSubsidyPayments
    )

  final val newEoriAndDanProvidedIfNeeded: Validate[J] =
    all(
      whenTrue(
        j => j.answers.basisOfClaim == Some(IncorrectEoriAndDan),
        checkIsTrue(
          _.answers.newEori.isDefined,
          MISSING_NEW_EORI
        )
      ),
      whenTrue(
        j => j.answers.basisOfClaim == Some(IncorrectEoriAndDan),
        checkIsTrue(
          _.answers.newDan.isDefined,
          MISSING_NEW_DAN
        )
      )
    )

}
