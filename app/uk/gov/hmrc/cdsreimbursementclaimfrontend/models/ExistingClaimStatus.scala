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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed class ExistingClaimStatus

object ExistingClaimStatus extends EnumerationFormat[ExistingClaimStatus] {
  case object Open extends ExistingClaimStatus
  case object PendingApproval extends ExistingClaimStatus
  case object PendingPayment extends ExistingClaimStatus
  case object PartialRefund extends ExistingClaimStatus
  case object ResolvedRefund extends ExistingClaimStatus
  case object ResolvedWithdrawn extends ExistingClaimStatus
  case object PendingQuery extends ExistingClaimStatus
  case object ResolvedManualBTA extends ExistingClaimStatus
  case object PendingC18 extends ExistingClaimStatus
  case object ClosedC18Raised extends ExistingClaimStatus
  case object RTBHLetterInitiated extends ExistingClaimStatus
  case object AwaitingRTBHLetterResponse extends ExistingClaimStatus
  case object ReminderLetterInitiated extends ExistingClaimStatus
  case object AwaitingReminderLetterResponse extends ExistingClaimStatus
  case object DecisionLetterInitiated extends ExistingClaimStatus
  case object PartialBTA extends ExistingClaimStatus
  case object PartialBTA_Refund extends ExistingClaimStatus
  case object ResolvedAutoBTA extends ExistingClaimStatus
  case object ResolvedManualBTA_Refund extends ExistingClaimStatus
  case object OpenExtensionGranted extends ExistingClaimStatus

  override val values: Set[ExistingClaimStatus] = Set(
    Open,
    PendingApproval,
    PendingPayment,
    PartialRefund,
    ResolvedRefund,
    ResolvedWithdrawn,
    PendingQuery,
    ResolvedManualBTA,
    PendingC18,
    ClosedC18Raised,
    RTBHLetterInitiated,
    AwaitingRTBHLetterResponse,
    ReminderLetterInitiated,
    AwaitingReminderLetterResponse,
    DecisionLetterInitiated,
    PartialBTA,
    PartialBTA_Refund,
    ResolvedAutoBTA,
    ResolvedManualBTA_Refund,
    OpenExtensionGranted
  )
}
