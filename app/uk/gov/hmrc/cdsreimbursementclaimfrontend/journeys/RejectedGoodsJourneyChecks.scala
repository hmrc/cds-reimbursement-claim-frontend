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

import com.github.arturopala.validator.Validator._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim

trait RejectedGoodsJourneyChecks[J <: RejectedGoodsJourneyProperties]
    extends CommonJourneyChecks[RejectedGoodsJourneyProperties] {

  final val basisOfClaimHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.basisOfClaim, MISSING_BASIS_OF_CLAIM)

  final val detailsOfRejectedGoodsHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.detailsOfRejectedGoods, MISSING_DETAILS_OF_REJECTED_GOODS)

  final val inspectionDateHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.inspectionDate, MISSING_INSPECTION_DATE)

  final val inspectionAddressHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.inspectionAddress, MISSING_INSPECTION_ADDRESS)

  final val methodOfDisposalHasBeenProvided: Validate[J] =
    checkIsDefined(_.answers.methodOfDisposal, MISSING_METHOD_OF_DISPOSAL)

  final val reimbursementClaimsHasBeenProvided: Validate[J] =
    all(
      checkIsTrue(_.hasCompleteReimbursementClaims, INCOMPLETE_REIMBURSEMENT_CLAIMS),
      checkIsTrue(_.getTotalReimbursementAmount > 0, TOTAL_REIMBURSEMENT_AMOUNT_MUST_BE_GREATER_THAN_ZERO)
    )

  final val basisOfClaimSpecialCircumstancesHasBeenProvidedIfNeeded: Validate[J] =
    all(
      whenTrue(
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        checkIsDefined(
          _.answers.basisOfClaimSpecialCircumstances,
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED
        )
      ),
      whenFalse(
        _.answers.basisOfClaim.contains(BasisOfRejectedGoodsClaim.SpecialCircumstances),
        checkIsEmpty(
          _.answers.basisOfClaimSpecialCircumstances,
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_NOT_BE_DEFINED
        )
      )
    )

  final def whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments: Validate[J] =
    whenTrue(
      journey => journey.features.exists(_.shouldBlockSubsidies) && !journey.isSubsidyOnlyJourney,
      declarationsHasNoSubsidyPayments
    )

}
