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

sealed trait BasisForClaim extends Product with Serializable {
  def repr: String
}

object BasisForClaim {
  case object DuplicateMrnEntry extends BasisForClaim {
    override def repr = "Duplicate Mrn or Entry number"
  }
  case object DutySuspension extends BasisForClaim {
    override def repr = "Duty Suspension"
  }
  case object EndUseRelief extends BasisForClaim {
    override def repr = "End Use Relief"
  }
  case object IncorrectCommodityCode extends BasisForClaim {
    override def repr = "Incorrect Commodity Code"
  }
  case object IncorrectCpc extends BasisForClaim {
    override def repr = "Incorrect Cpc"
  }
  case object IncorrectValue extends BasisForClaim {
    override def repr = "Incorrect Value"
  }
  case object IncorrectEoriAndDefermentAccountNumber extends BasisForClaim {
    override def repr = "Incorrect Eori and Deferment Account Number"
  }

  case object InwardProcessingReliefFromCustomsDuty extends BasisForClaim {
    override def repr = "Inward Processing Relief from Customs Duty"
  }
  case object OutwardProcessingRelief extends BasisForClaim {
    override def repr = "Outward Processing Relief"
  }
  case object Preference extends BasisForClaim {
    override def repr = "Preference"
  }
  case object ProofOfReturnRefundGiven extends BasisForClaim {
    override def repr = "Proof of Return and Refund Given"
  }

  implicit val format: OFormat[BasisForClaim] = derived.oformat[BasisForClaim]()

}
