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

import cats.kernel.Eq
import play.api.libs.json._
import julienrf.json.derived

sealed abstract class BasisOfClaim(val value: Int, val string: String) extends Product with Serializable

object BasisOfClaim {
  case object DuplicateEntry extends BasisOfClaim(0, "Duplicate Entry")
  case object DutySuspension extends BasisOfClaim(1, "Duty Suspension")
  case object EndUseRelief extends BasisOfClaim(2, "End Use")
  case object IncorrectCommodityCode extends BasisOfClaim(3, "Incorrect Commodity Code")
  case object IncorrectCpc extends BasisOfClaim(4, "Incorrect CPC")
  case object IncorrectValue extends BasisOfClaim(5, "Incorrect Value")
  case object IncorrectEoriAndDefermentAccountNumber extends BasisOfClaim(6, "Incorrect EORI & Deferment Acc. Num.")
  case object InwardProcessingReliefFromCustomsDuty extends BasisOfClaim(7, "IP")
  case object OutwardProcessingRelief extends BasisOfClaim(8, "OPR")
  case object PersonalEffects extends BasisOfClaim(9, "Personal Effects")
  case object Preference extends BasisOfClaim(10, "Preference")
  case object RGR extends BasisOfClaim(11, "RGR")
  case object ProofOfReturnRefundGiven extends BasisOfClaim(12, "Proof of Return/Refund Given")
  case object IncorrectExciseValue extends BasisOfClaim(13, "Incorrect excise value") //Northern Ireland only
  case object CorrectionToRiskClassification
      extends BasisOfClaim(14, "Correction to risk classification") //Northern Ireland only
  case object Miscellaneous extends BasisOfClaim(15, "Miscellaneous")

  val allClaimsTypes: List[BasisOfClaim]               = List(
    DuplicateEntry,
    DutySuspension,
    EndUseRelief,
    IncorrectCommodityCode,
    IncorrectCpc,
    IncorrectValue,
    IncorrectEoriAndDefermentAccountNumber,
    InwardProcessingReliefFromCustomsDuty,
    OutwardProcessingRelief,
    PersonalEffects,
    Preference,
    RGR,
    ProofOfReturnRefundGiven,
    IncorrectExciseValue,
    CorrectionToRiskClassification,
    Miscellaneous
  )
  val allClaimsIntToType: Map[Int, BasisOfClaim]       = allClaimsTypes.map(a => a.value -> a).toMap
  val allClaimsTypeToInt: Map[BasisOfClaim, Int]       = allClaimsTypes.map(a => a -> a.value).toMap
  val allClaimsStringToType: Map[String, BasisOfClaim] = allClaimsTypes.map(a => a.string -> a).toMap

  val allClaimsTypesPartialFunctions: List[PartialFunction[BasisOfClaim, String]] = allClaimsTypes.map(a =>
    new PartialFunction[BasisOfClaim, String]() {
      def apply(v1: BasisOfClaim): String       = a.string
      def isDefinedAt(x: BasisOfClaim): Boolean = true
    }
  )

  implicit def classToNameString(in: BasisOfClaim): String =
    allClaimsTypesPartialFunctions.drop(1).foldLeft(allClaimsTypesPartialFunctions(0))(_ orElse _)(in)

  implicit val eq: Eq[BasisOfClaim] = Eq.fromUniversalEquals[BasisOfClaim]

  implicit val basisOfClaimFormat: OFormat[BasisOfClaim] = derived.oformat[BasisOfClaim]()
}
