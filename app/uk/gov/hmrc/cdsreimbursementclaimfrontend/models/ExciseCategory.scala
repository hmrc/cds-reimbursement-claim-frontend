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

sealed abstract class ExciseCategory(val repr: String, val ordinal: Int) {
  lazy val taxCodes: Seq[TaxCode] = TaxCodes.excise.filter(_.exciseCategory == Some(this))
}

object ExciseCategory {

  val simpleExciseCategoryFormat: Format[ExciseCategory] =
    SimpleStringFormat[ExciseCategory](
      repr =>
        ExciseCategory
          .find(repr)
          .getOrElse(throw new Exception(s"Cannot parse excise category from the string [$repr]")),
      _.repr
    )

  implicit val format: Format[ExciseCategory]       = simpleExciseCategoryFormat
  implicit val exciseCategoryEq: Eq[ExciseCategory] = Eq.fromUniversalEquals[ExciseCategory]
  implicit val ordering: Ordering[ExciseCategory]   = Ordering.by(_.ordinal)

  def apply(value: String): ExciseCategory =
    ExciseCategory.findUnsafe(value)

  def unapply(exciseCategory: ExciseCategory): Option[String] =
    Some(exciseCategory.repr)

  case object Beer extends ExciseCategory("beer", 2)
  case object Wine extends ExciseCategory("wine", 3)
  case object MadeWine extends ExciseCategory("made-wine", 4)
  case object LowAlcoholBeverages extends ExciseCategory("low-alcohol-beverages", 5)
  case object Spirits extends ExciseCategory("spirits", 6)
  case object CiderPerry extends ExciseCategory("cider-perry", 7)
  case object OtherFermentedProducts extends ExciseCategory("other-fermented-products", 8)
  case object HydrocarbonOils extends ExciseCategory("hydrocarbon-oils", 9)
  case object Biofuels extends ExciseCategory("biofuels", 10)
  case object MiscellaneousRoadFuels extends ExciseCategory("miscellaneous-road-fuels", 11)
  case object Tobacco extends ExciseCategory("tobacco", 12)
  case object ClimateChangeLevy extends ExciseCategory("climate-change-levy", 13)

  val all: Seq[ExciseCategory] = List(
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

  private lazy val exciseCategorysStringMap: Map[String, ExciseCategory] =
    all.map(exciseCategory => exciseCategory.repr -> exciseCategory).toMap

  private lazy val taxCode2ExciseCategoryMap: Map[TaxCode, ExciseCategory] =
    val m = collection.mutable.Map.empty[TaxCode, ExciseCategory]
    all.foreach(ec =>
      ec.taxCodes.foreach { tc =>
        m.get(tc).match {
          case Some(existing) =>
            throw new Exception(
              s"${scala.io.AnsiColor.RED}TaxCode ${scala.io.AnsiColor.BOLD}$tc${scala.io.AnsiColor.RESET}${scala.io.AnsiColor.RED} belongs to more then one excise category $ec and $existing${scala.io.AnsiColor.RESET}"
            )

          case None =>
            m.put(tc, ec)
        }
      }
    )
    m.toMap

  def categoryOf(taxCode: TaxCode): ExciseCategory =
    taxCode2ExciseCategoryMap(taxCode)

  def find(representation: String): Option[ExciseCategory] =
    exciseCategorysStringMap.get(representation)

  def findUnsafe(representation: String): ExciseCategory =
    exciseCategorysStringMap(representation)

  def has(code: String): Boolean =
    exciseCategorysStringMap.contains(code)

}
