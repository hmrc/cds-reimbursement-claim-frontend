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
import cats.syntax.eq._
import julienrf.json.derived
import play.api.libs.json.OFormat
import play.api.mvc.PathBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode._

sealed abstract class DutyType(val value: String) extends Product with Serializable

object DutyType {

  case object UkDuty extends DutyType("uk-duty")
  case object EuDuty extends DutyType("eu-duty")
  case object Beer extends DutyType("beer")
  case object Wine extends DutyType("wine")
  case object MadeWine extends DutyType("made-wine")
  case object LowAlcoholBeverages extends DutyType("low-alcohol-beverages")
  case object Spirits extends DutyType("spirits")
  case object CiderPerry extends DutyType("cider-and-perry")
  case object HydrocarbonOils extends DutyType("hydrocarbon-oils")
  case object Biofuels extends DutyType("biofuels")
  case object MiscellaneousRoadFuels extends DutyType("miscellaneous-road-fuels")
  case object Tobacco extends DutyType("tobacco-products")
  case object ClimateChangeLevy extends DutyType("climate-change-levy")

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

  val categoryToTaxCode: Map[DutyType, List[TaxCode]] = Map(
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

  val allDutyTypesMap: Map[String, DutyType] = dutyTypes.map(a => a.value -> a).toMap

  def validateTaxCategoryAndCode(category: DutyType, code: TaxCode): Boolean =
    categoryToTaxCode.get(category).map(_.exists(_ === code)).getOrElse(false)

  implicit val eq: Eq[TaxCode] = Eq.fromUniversalEquals[TaxCode]

  def parse(str: String): Either[String, DutyType] =
    dutyTypes.find(a => a.value === str).toRight("No such category")

  implicit lazy val taxCategoryBindable: PathBindable[DutyType] = new PathBindable[DutyType] {

    override def bind(key: String, value: String): Either[String, DutyType] =
      parse(value)

    override def unbind(key: String, bindable: DutyType): String =
      bindable.value
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
