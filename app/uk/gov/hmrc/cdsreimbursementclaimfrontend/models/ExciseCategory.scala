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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.kernel.Eq
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

sealed abstract class ExciseCategory(val repr: String, val taxCodes: Seq[TaxCode], val ordinal: Int)
    extends Product
    with Serializable

object ExciseCategory {

  implicit val format: Format[ExciseCategory]       = simpleExciseCategoryFormat
  implicit val exciseCategoryEq: Eq[ExciseCategory] = Eq.fromUniversalEquals[ExciseCategory]
  implicit val ordering: Ordering[ExciseCategory]   = Ordering.by(_.ordinal)

  def apply(value: String): ExciseCategory =
    ExciseCategory.findUnsafe(value)

  def unapply(exciseCategory: ExciseCategory): Option[String] =
    Some(exciseCategory.repr)

  val all: List[ExciseCategory] = List(
    Beer,
    Wine,
    MadeWine,
    LowAlcoholBeverages,
    Spirits,
    CiderPerry,
    OtherFermentedProducts,
    HydrocarbonOils,
    Biofuels,
    MiscellaneousRoadFuels,
    Tobacco,
    ClimateChangeLevy
  ).sorted

  case object Beer
      extends ExciseCategory(
        "beer",
        List(
          NI311,
          NI321,
          NI331,
          NI341,
          NI351,
          NI356,
          NI361,
          NI366,
          NI371,
          NI376,
          NI407,
          NI440,
          NI441,
          NI442,
          NI443,
          NI444,
          NI445,
          NI446,
          NI447,
          NI473
        ),
        2
      )

  case object Wine
      extends ExciseCategory(
        "wine",
        List(NI313, NI323, NI333, NI343, NI353, NI358, NI363, NI368, NI373, NI378, NI411, NI412, NI413, NI415, NI419),
        3
      )

  case object MadeWine extends ExciseCategory("made-wine", List(NI421, NI422, NI423, NI425, NI429), 4)
  case object LowAlcoholBeverages
      extends ExciseCategory("low-alcohol-beverages", List(NI301, NI431, NI433, NI435, NI444, NI446, NI473), 5)

  case object Spirits
      extends ExciseCategory(
        "spirits",
        List(NI315, NI325, NI335, NI345, NI355, NI360, NI365, NI370, NI375, NI380, NI438, NI451, NI461, NI462, NI463),
        6
      )

  case object CiderPerry
      extends ExciseCategory(
        "cider-perry",
        List(NI312, NI322, NI352, NI357, NI362, NI367, NI372, NI377, NI431, NI481, NI483, NI485, NI487),
        7
      )

  case object OtherFermentedProducts
      extends ExciseCategory(
        "other-fermented-products",
        List(NI314, NI324, NI334, NI344, NI354, NI359, NI364, NI369, NI374, NI379),
        8
      )

  case object HydrocarbonOils
      extends ExciseCategory(
        "hydrocarbon-oils",
        List(
          NI511,
          NI520,
          NI521,
          NI522,
          NI540,
          NI541,
          NI542,
          NI546,
          NI551,
          NI556,
          NI561,
          NI570,
          NI571,
          NI572
        ),
        9
      )

  case object Biofuels extends ExciseCategory("biofuels", List(NI589, NI595, NI597), 10)

  case object MiscellaneousRoadFuels extends ExciseCategory("miscellaneous-road-fuels", List(NI591, NI592), 11)

  case object Tobacco extends ExciseCategory("tobacco", List(NI611, NI615, NI619, NI623, NI627, NI633), 12)

  case object ClimateChangeLevy extends ExciseCategory("climate-change-levy", List(NI99A, NI99B, NI99C, NI99D), 13)

  val simpleExciseCategoryFormat: Format[ExciseCategory] =
    SimpleStringFormat[ExciseCategory](
      repr =>
        ExciseCategory
          .find(repr)
          .getOrElse(throw new Exception(s"Cannot parse excise category from the string [$repr]")),
      _.repr
    )

  private lazy val exciseCategorysStringMap: Map[String, ExciseCategory] =
    all.map(exciseCategory => exciseCategory.repr -> exciseCategory).toMap

  private lazy val taxCode2ExciseCategoryMap: Map[TaxCode, ExciseCategory] =
    all.flatMap(dt => dt.taxCodes.map(tc => (tc, dt))).toMap

  def categoryOf(taxCode: TaxCode): ExciseCategory =
    taxCode2ExciseCategoryMap(taxCode)

  def find(representation: String): Option[ExciseCategory] =
    exciseCategorysStringMap.get(representation)

  def findUnsafe(representation: String): ExciseCategory =
    exciseCategorysStringMap(representation)
}
