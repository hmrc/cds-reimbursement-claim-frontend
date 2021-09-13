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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement

import cats.kernel.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode._

import java.util.Locale

sealed trait DutyType extends Product with Serializable

object DutyType {

  case object UkDuty extends DutyType
  case object EuDuty extends DutyType
  case object Beer extends DutyType
  case object Wine extends DutyType
  case object MadeWine extends DutyType
  case object LowAlcoholBeverages extends DutyType
  case object Spirits extends DutyType
  case object CiderPerry extends DutyType
  case object HydrocarbonOils extends DutyType
  case object Biofuels extends DutyType
  case object MiscellaneousRoadFuels extends DutyType
  case object Tobacco extends DutyType
  case object ClimateChangeLevy extends DutyType

  val dutyTypes: List[DutyType] = List(
    UkDuty,
    EuDuty,
    Beer,
    Wine,
    MadeWine,
    LowAlcoholBeverages,
    Spirits,
    CiderPerry,
    HydrocarbonOils,
    Biofuels,
    MiscellaneousRoadFuels,
    Tobacco,
    ClimateChangeLevy
  )

  val customDutyTypes: List[DutyType] = List(
    UkDuty,
    EuDuty
  )

  val exciseDutyTypes: List[DutyType] = List(
    Beer,
    Wine,
    MadeWine,
    LowAlcoholBeverages,
    Spirits,
    CiderPerry,
    HydrocarbonOils,
    Biofuels,
    MiscellaneousRoadFuels,
    Tobacco,
    ClimateChangeLevy
  )

  val dutyTypeToTaxCodes: Map[DutyType, List[TaxCode]] = Map(
    UkDuty                 -> TaxCode.listOfUKTaxCodes,
    EuDuty                 -> TaxCode.listOfEUTaxCodes,
    Beer                   -> List(NI407, NI440, NI441, NI442, NI443, NI444, NI445, NI446, NI447, NI473),
    Wine                   -> List(NI411, NI412, NI413, NI415, NI419), //TODO potential problem with 411 appearing twice!
    MadeWine               -> List(NI421, NI422, NI423, NI425, NI429),
    LowAlcoholBeverages    -> List(NI431, NI433, NI435, NI473),
    Spirits                -> List(NI438, NI451, NI461, NI462, NI463),
    CiderPerry             -> List(NI431, NI481, NI483, NI485, NI487),
    HydrocarbonOils        -> List(
      NI511,
      NI511,
      NI520,
      NI521,
      NI522,
      NI540,
      NI541,
      NI542,
      NI551,
      NI556,
      NI561,
      NI570,
      NI571,
      NI572
    ),
    Biofuels               -> List(NI589, NI595),
    MiscellaneousRoadFuels -> List(NI591, NI592),
    Tobacco                -> List(NI611, NI615, NI619, NI623, NI627, NI633),
    ClimateChangeLevy      -> List(NI99A, NI99B, NI99C, NI99D)
  )

  def toDutyType(repr: String): Option[DutyType] =
    repr match {
      case "ukduty"                 => Some(UkDuty)
      case "euduty"                 => Some(EuDuty)
      case "beer"                   => Some(Beer)
      case "wine"                   => Some(Wine)
      case "madewine"               => Some(MadeWine)
      case "lowalcoholbeverages"    => Some(LowAlcoholBeverages)
      case "spirits"                => Some(Spirits)
      case "ciderperry"             => Some(CiderPerry)
      case "hydrocarbonoils"        => Some(HydrocarbonOils)
      case "biofuels"               => Some(Biofuels)
      case "miscellaneousroadfuels" => Some(MiscellaneousRoadFuels)
      case "tobacco"                => Some(Tobacco)
      case "climatechangelevy"      => Some(ClimateChangeLevy)
      case _                        => None
    }

  def typeToString(dutyType: DutyType): String =
    dutyType match {
      case UkDuty                 => UkDuty.toString.toLowerCase(Locale.UK)
      case EuDuty                 => EuDuty.toString.toLowerCase(Locale.UK)
      case Beer                   => Beer.toString.toLowerCase(Locale.UK)
      case Wine                   => Wine.toString.toLowerCase(Locale.UK)
      case MadeWine               => MadeWine.toString.toLowerCase(Locale.UK)
      case LowAlcoholBeverages    => LowAlcoholBeverages.toString.toLowerCase(Locale.UK)
      case Spirits                => Spirits.toString.toLowerCase(Locale.UK)
      case CiderPerry             => CiderPerry.toString.toLowerCase(Locale.UK)
      case HydrocarbonOils        => HydrocarbonOils.toString.toLowerCase(Locale.UK)
      case Biofuels               => Biofuels.toString.toLowerCase(Locale.UK)
      case MiscellaneousRoadFuels => MiscellaneousRoadFuels.toString.toLowerCase(Locale.UK)
      case Tobacco                => Tobacco.toString.toLowerCase(Locale.UK)
      case ClimateChangeLevy      => ClimateChangeLevy.toString.toLowerCase(Locale.UK)
    }

  implicit val taxCategoryEq: Eq[DutyType] = Eq.fromUniversalEquals[DutyType]

  implicit val format: OFormat[DutyType] = derived.oformat[DutyType]()

  def apply(value: Int): DutyType = DutyType.dutyTypes(value)

  def unapply(dutyType: DutyType): Option[Int] = {
    val index = DutyType.dutyTypes.indexOf(dutyType)
    if (index > -1) {
      Some(index)
    } else {
      None
    }
  }

}
