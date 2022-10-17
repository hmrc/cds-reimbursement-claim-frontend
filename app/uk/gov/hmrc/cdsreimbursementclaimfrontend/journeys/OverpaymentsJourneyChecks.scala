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

import JourneyValidationErrors._
import com.github.arturopala.validator.Validator._

trait OverpaymentsJourneyChecks[Journey <: OverpaymentsJourneyProperties]
    extends CommonJourneyChecks[OverpaymentsJourneyProperties] {

  final val basisOfClaimHasBeenProvided: Validate[Journey] =
    checkIsDefined(_.answers.basisOfClaim, MISSING_BASIS_OF_CLAIM)

  final val additionalDetailsHasBeenProvided: Validate[Journey] =
    checkIsDefined(_.answers.additionalDetails, MISSING_DETAILS_OF_OVERPAYMENT)

  final val reimbursementClaimsHasBeenProvided: Validate[Journey] =
    all(
      checkIsTrue(_.hasCompleteReimbursementClaims, INCOMPLETE_REIMBURSEMENT_CLAIMS),
      checkIsTrue(_.getTotalReimbursementAmount > 0, TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO)
    )

}