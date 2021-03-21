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

sealed abstract class BasisOfClaim(val intValue: Int, val stringValue: String) extends Product with Serializable

object BasisOfClaim {
  case object DuplicateEntry extends BasisOfClaim(0, "Duplicate Entry")
  case object DutySuspension extends BasisOfClaim(1, "Duty Suspension")
  case object EndUseRelief extends BasisOfClaim(2, "End Use")
  case object IncorrectCommodityCode extends BasisOfClaim(3, "Incorrect Commodity Code")
  case object IncorrectCpc extends BasisOfClaim(4, "Incorrect CPC")
  case object IncorrectValue extends BasisOfClaim(5, "Incorrect Value")
  case object IncorrectEoriAndDefermentAccountNumber extends BasisOfClaim(6, "Incorrect EORI & Deferment Acc. Num.")
  case object InwardProcessingReliefFromCustomsDuty extends BasisOfClaim(7, "IP")
  case object Miscellaneous extends BasisOfClaim(8, "Miscellaneous")
  case object OutwardProcessingRelief extends BasisOfClaim(9, "OPR")
  case object PersonalEffects extends BasisOfClaim(10, "Personal Effects")
  case object Preference extends BasisOfClaim(11, "Preference")
  case object RGR extends BasisOfClaim(12, "RGR")
  case object ProofOfReturnRefundGiven extends BasisOfClaim(13, "Proof of Return/Refund Given")

  val allClaims: List[BasisOfClaim] = List(
    DuplicateEntry,
    DutySuspension,
    EndUseRelief,
    IncorrectCommodityCode,
    IncorrectCpc,
    IncorrectValue,
    IncorrectEoriAndDefermentAccountNumber,
    InwardProcessingReliefFromCustomsDuty,
    Miscellaneous,
    OutwardProcessingRelief,
    PersonalEffects,
    Preference,
    RGR,
    ProofOfReturnRefundGiven
  )

  val int2ClaimMap: Map[Int, BasisOfClaim] = allClaims.map(claim => (claim.intValue, claim)).toMap
  val claim2IntMap: Map[BasisOfClaim, Int] = allClaims.map(claim => (claim, claim.intValue)).toMap
  val intValues: List[Int]                 = allClaims.map(_.intValue)

  implicit val format: OFormat[BasisOfClaim] = derived.oformat[BasisOfClaim]()

}
