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

import cats.Eq
import cats.syntax.eq._
import play.api.mvc.PathBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode._

sealed abstract class TaxCategory(val value: String) extends Product with Serializable

object TaxCategory {

  case object UkDuty extends TaxCategory("uk-duty")
  case object EuDuty extends TaxCategory("eu-duty")
  case object Beer extends TaxCategory("beer")
  case object Wine extends TaxCategory("wine")
  case object MadeWine extends TaxCategory("made-wine")
  case object LowAlcoholBeverages extends TaxCategory("low-alcohol-beverages")
  case object Spirits extends TaxCategory("spirits")
  case object CiderPerry extends TaxCategory("cider-perry")
  case object HydrocarbonOils extends TaxCategory("hydrocarbon-oils")
  case object Biofuels extends TaxCategory("biofuels")
  case object MiscellaneousRoadFuels extends TaxCategory("miscellaneous-road-fuels")
  case object Tobacco extends TaxCategory("tobacco")
  case object ClimateChangeLevy extends TaxCategory("climate-change-levy")

  val dutyCategoriesList: List[TaxCategory] = List(
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

  val categoryToTaxCode: Map[TaxCategory, List[TaxCode]] = Map(
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

  def validateTaxCategoryAndCode(category: TaxCategory, code: TaxCode): Boolean =
    categoryToTaxCode.get(category).map(_.exists(_ === code)).getOrElse(false)

  implicit val eq: Eq[TaxCode] = Eq.fromUniversalEquals[TaxCode]

  def parse(str: String): Either[String, TaxCategory] =
    dutyCategoriesList.find(a => a.value === str).toRight("No such category")

  implicit lazy val taxCategoryBindable: PathBindable[TaxCategory] = new PathBindable[TaxCategory] {

    override def bind(key: String, value: String): Either[String, TaxCategory] =
      parse(value)

    override def unbind(key: String, bindable: TaxCategory): String =
      bindable.value
  }

}
