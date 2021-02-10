/*
 * Copyright 2021 HM Revenue & Customs
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

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait ReasonForClaimOption extends Product with Serializable

object ReasonForClaimOption {
  case object DuplicateMrnEntry extends ReasonForClaimOption
  case object DutySuspension extends ReasonForClaimOption
  case object EndUseRelief extends ReasonForClaimOption
  case object IncorrectCommodityCode extends ReasonForClaimOption
  case object IncorrectCpc extends ReasonForClaimOption
  case object IncorrectValue extends ReasonForClaimOption
  case object IncorrectEoriAndDefermentAccountNumber extends ReasonForClaimOption
  case object InwardProcessingReliefFromCustomsDuty extends ReasonForClaimOption
  case object OutwardProcessingRelief extends ReasonForClaimOption
  case object Preference extends ReasonForClaimOption
  case object ProofOfReturnRefundGiven extends ReasonForClaimOption

  implicit val format: OFormat[ReasonForClaimOption] = derived.oformat[ReasonForClaimOption]()

}
