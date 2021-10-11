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

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat
import play.api.mvc.PathBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode._

sealed abstract class DutyType(val repr: String, val taxCodes: List[TaxCode]) extends Product with Serializable

object DutyType {

  case object UkDuty extends DutyType("uk-duty", TaxCode.listOfUKTaxCodes)

  case object EuDuty extends DutyType("eu-duty", TaxCode.listOfEUTaxCodes)

  case object Beer extends DutyType("beer", List(NI407, NI440, NI441, NI442, NI443, NI444, NI445, NI446, NI447, NI473))

  case object Wine extends DutyType("wine", List(NI411, NI412, NI413, NI415, NI419))

  case object Biofuels extends DutyType("biofuels", List(NI589, NI595))

  case object MiscellaneousRoadFuels extends DutyType("miscellaneous-road-fuels", List(NI591, NI592))

  case object Tobacco extends DutyType("tobacco", List(NI611, NI615, NI619, NI623, NI627, NI633))

  case object ClimateChangeLevy extends DutyType("climate-change-levy", List(NI99A, NI99B, NI99C, NI99D))

  case object MadeWine extends DutyType("made-wine", List(NI421, NI422, NI423, NI425, NI429))

  case object Spirits extends DutyType("spirits", List(NI438, NI451, NI461, NI462, NI463))

  case object CiderPerry extends DutyType("cider-perry", List(NI431, NI481, NI483, NI485, NI487))

  case object LowAlcoholBeverages
      extends DutyType("low-alcohol-beverages", List(NI431, NI433, NI435, NI444, NI446, NI473))

  case object HydrocarbonOils
      extends DutyType(
        "hydrocarbon-oils",
        List(
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
        )
      )

  val dutyTypeToRankMap: Map[DutyType, Int] = DutyTypes.all.zipWithIndex.toMap

  def toDutyType(repr: String): Option[DutyType] = DutyTypes.all.find(_.repr === repr)

  def typeToString(dutyType: DutyType): String = dutyType.repr

  implicit val taxCategoryEq: Eq[DutyType] = Eq.fromUniversalEquals[DutyType]

  implicit val format: OFormat[DutyType] = derived.oformat[DutyType]()

  def apply(value: String): DutyType =
    DutyType.toDutyType(value).getOrElse(sys.error(s"Could not map to a duty type : $value"))

  implicit val binder: PathBindable[DutyType] =
    new PathBindable[DutyType] {
      val stringBinder: PathBindable[String] = implicitly[PathBindable[String]]

      override def bind(key: String, value: String): Either[String, DutyType] =
        stringBinder.bind(key, value).map(DutyType.apply)

      override def unbind(key: String, dutyType: DutyType): String =
        stringBinder.unbind(key, dutyType.repr)
    }

  def unapply(dutyType: DutyType): Option[String] = Some(dutyType.repr)

  object DutyTypeOrdering extends Ordering[DutyType] {
    def compare(a: DutyType, b: DutyType): Int =
      DutyType.dutyTypeToRankMap(a) compare DutyType.dutyTypeToRankMap(b)
  }
}
