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

sealed trait BasisOfClaim extends Product with Serializable

object BasisOfClaim {
  case object DuplicateEntry extends BasisOfClaim
  case object DutySuspension extends BasisOfClaim
  case object EndUseRelief extends BasisOfClaim
  case object IncorrectCommodityCode extends BasisOfClaim
  case object IncorrectCpc extends BasisOfClaim
  case object IncorrectValue extends BasisOfClaim
  case object IncorrectEoriAndDefermentAccountNumber extends BasisOfClaim
  case object InwardProcessingReliefFromCustomsDuty extends BasisOfClaim
  case object Miscellaneous extends BasisOfClaim
  case object OutwardProcessingRelief extends BasisOfClaim
  case object PersonalEffects extends BasisOfClaim
  case object Preference extends BasisOfClaim
  case object RGR extends BasisOfClaim
  case object ProofOfReturnRefundGiven extends BasisOfClaim

  implicit def basisForClaimToString(basisForClaim: BasisOfClaim): String = basisForClaim match {
    case DuplicateEntry                         => "Duplicate Entry"
    case DutySuspension                         => "Duty Suspension"
    case EndUseRelief                           => "End Use"
    case IncorrectCommodityCode                 => "Incorrect Commodity Code"
    case IncorrectCpc                           => "Incorrect CPC"
    case IncorrectValue                         => "Incorrect Value"
    case IncorrectEoriAndDefermentAccountNumber => "Incorrect EORI & Deferment Acc. Num."
    case InwardProcessingReliefFromCustomsDuty  => "IP"
    case Miscellaneous                          => "Miscellaneous"
    case OutwardProcessingRelief                => "OPR"
    case PersonalEffects                        => "Personal Effects"
    case Preference                             => "Preference"
    case RGR                                    => "RGR"
    case ProofOfReturnRefundGiven               => "Proof of Return/Refund Given"
  }

  implicit val format: OFormat[BasisOfClaim] = derived.oformat[BasisOfClaim]()

}
