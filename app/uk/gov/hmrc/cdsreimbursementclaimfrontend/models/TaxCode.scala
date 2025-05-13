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

import cats.Eq
import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

sealed abstract class TaxCode(
  val value: String,
  val dutyType: DutyType,
  val exciseCategory: Option[ExciseCategory] = None,
  val isSubsidy: Boolean = false,
  val isVAT: Boolean = false
) extends Product
    with Serializable {
  override def toString: String = value
}

object TaxCode {

  def apply(value: String): TaxCode =
    TaxCodes.findUnsafe(value)

  def unapply(taxCode: TaxCode): Option[String] =
    Some(taxCode.value)

  final case class UnsupportedTaxCode(taxCode: String) extends TaxCode(taxCode, DutyType.Unknown)

  case object A00 extends TaxCode("A00", DutyType.UkDuty)
  case object A20 extends TaxCode("A20", DutyType.UkDuty)
  case object A30 extends TaxCode("A30", DutyType.UkDuty)
  case object A35 extends TaxCode("A35", DutyType.UkDuty)
  case object A40 extends TaxCode("A40", DutyType.UkDuty)
  case object A45 extends TaxCode("A45", DutyType.UkDuty)
  case object B00 extends TaxCode("B00", DutyType.UkDuty, isVAT = true)

  case object A50 extends TaxCode("A50", DutyType.EuDuty, isSubsidy = true)
  case object A70 extends TaxCode("A70", DutyType.EuDuty, isSubsidy = true)
  case object A80 extends TaxCode("A80", DutyType.EuDuty, isSubsidy = true)
  case object A85 extends TaxCode("A85", DutyType.EuDuty)
  case object A90 extends TaxCode("A90", DutyType.EuDuty, isSubsidy = true)
  case object A95 extends TaxCode("A95", DutyType.EuDuty)
  case object B05 extends TaxCode("B05", DutyType.EuDuty, isVAT = true)

  case object NI311 extends TaxCode("311", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI321 extends TaxCode("321", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI331 extends TaxCode("331", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI341 extends TaxCode("341", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI351 extends TaxCode("351", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI356 extends TaxCode("356", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI361 extends TaxCode("361", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI366 extends TaxCode("366", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI371 extends TaxCode("371", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI376 extends TaxCode("376", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI407 extends TaxCode("407", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI440 extends TaxCode("440", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI441 extends TaxCode("441", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI442 extends TaxCode("442", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI443 extends TaxCode("443", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI445 extends TaxCode("445", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI447 extends TaxCode("447", DutyType.Excise, Some(ExciseCategory.Beer))
  case object NI313 extends TaxCode("313", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI323 extends TaxCode("323", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI333 extends TaxCode("333", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI343 extends TaxCode("343", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI353 extends TaxCode("353", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI358 extends TaxCode("358", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI363 extends TaxCode("363", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI368 extends TaxCode("368", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI373 extends TaxCode("373", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI378 extends TaxCode("378", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI411 extends TaxCode("411", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI412 extends TaxCode("412", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI413 extends TaxCode("413", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI415 extends TaxCode("415", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI419 extends TaxCode("419", DutyType.Excise, Some(ExciseCategory.Wine))
  case object NI421 extends TaxCode("421", DutyType.Excise, Some(ExciseCategory.MadeWine))
  case object NI422 extends TaxCode("422", DutyType.Excise, Some(ExciseCategory.MadeWine))
  case object NI423 extends TaxCode("423", DutyType.Excise, Some(ExciseCategory.MadeWine))
  case object NI425 extends TaxCode("425", DutyType.Excise, Some(ExciseCategory.MadeWine))
  case object NI429 extends TaxCode("429", DutyType.Excise, Some(ExciseCategory.MadeWine))
  case object NI301 extends TaxCode("301", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI431 extends TaxCode("431", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI433 extends TaxCode("433", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI435 extends TaxCode("435", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI444 extends TaxCode("444", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI446 extends TaxCode("446", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI473 extends TaxCode("473", DutyType.Excise, Some(ExciseCategory.LowAlcoholBeverages))
  case object NI315 extends TaxCode("315", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI325 extends TaxCode("325", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI335 extends TaxCode("335", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI345 extends TaxCode("345", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI355 extends TaxCode("355", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI360 extends TaxCode("360", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI365 extends TaxCode("365", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI370 extends TaxCode("370", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI375 extends TaxCode("375", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI380 extends TaxCode("380", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI438 extends TaxCode("438", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI451 extends TaxCode("451", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI461 extends TaxCode("461", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI462 extends TaxCode("462", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI463 extends TaxCode("463", DutyType.Excise, Some(ExciseCategory.Spirits))
  case object NI312 extends TaxCode("312", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI322 extends TaxCode("322", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI352 extends TaxCode("352", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI357 extends TaxCode("357", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI362 extends TaxCode("362", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI367 extends TaxCode("367", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI372 extends TaxCode("372", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI377 extends TaxCode("377", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI481 extends TaxCode("481", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI483 extends TaxCode("483", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI485 extends TaxCode("485", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI487 extends TaxCode("487", DutyType.Excise, Some(ExciseCategory.CiderPerry))
  case object NI314 extends TaxCode("314", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI324 extends TaxCode("324", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI334 extends TaxCode("334", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI344 extends TaxCode("344", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI354 extends TaxCode("354", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI359 extends TaxCode("359", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI364 extends TaxCode("364", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI369 extends TaxCode("369", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI374 extends TaxCode("374", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI379 extends TaxCode("379", DutyType.Excise, Some(ExciseCategory.OtherFermentedProducts))
  case object NI511 extends TaxCode("511", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI520 extends TaxCode("520", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI521 extends TaxCode("521", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI522 extends TaxCode("522", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI540 extends TaxCode("540", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI541 extends TaxCode("541", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI542 extends TaxCode("542", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI546 extends TaxCode("546", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI551 extends TaxCode("551", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI556 extends TaxCode("556", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI561 extends TaxCode("561", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI570 extends TaxCode("570", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI571 extends TaxCode("571", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI572 extends TaxCode("572", DutyType.Excise, Some(ExciseCategory.HydrocarbonOils))
  case object NI589 extends TaxCode("589", DutyType.Excise, Some(ExciseCategory.Biofuels))
  case object NI595 extends TaxCode("595", DutyType.Excise, Some(ExciseCategory.Biofuels))
  case object NI597 extends TaxCode("597", DutyType.Excise, Some(ExciseCategory.Biofuels))
  case object NI591 extends TaxCode("591", DutyType.Excise, Some(ExciseCategory.MiscellaneousRoadFuels))
  case object NI592 extends TaxCode("592", DutyType.Excise, Some(ExciseCategory.MiscellaneousRoadFuels))
  case object NI611 extends TaxCode("611", DutyType.Excise, Some(ExciseCategory.Tobacco))
  case object NI615 extends TaxCode("615", DutyType.Excise, Some(ExciseCategory.Tobacco))
  case object NI619 extends TaxCode("619", DutyType.Excise, Some(ExciseCategory.Tobacco))
  case object NI623 extends TaxCode("623", DutyType.Excise, Some(ExciseCategory.Tobacco))
  case object NI627 extends TaxCode("627", DutyType.Excise, Some(ExciseCategory.Tobacco))
  case object NI633 extends TaxCode("633", DutyType.Excise, Some(ExciseCategory.Tobacco))
  case object NI99A extends TaxCode("99A", DutyType.Excise, Some(ExciseCategory.ClimateChangeLevy))
  case object NI99B extends TaxCode("99B", DutyType.Excise, Some(ExciseCategory.ClimateChangeLevy))
  case object NI99C extends TaxCode("99C", DutyType.Excise, Some(ExciseCategory.ClimateChangeLevy))
  case object NI99D extends TaxCode("99D", DutyType.Excise, Some(ExciseCategory.ClimateChangeLevy))

  implicit val taxCodeEq: Eq[TaxCode]         = Eq.fromUniversalEquals[TaxCode]
  implicit val taxCodeFormat: Format[TaxCode] = SimpleStringFormat(TaxCode(_), _.value)

  private inline def hash(tc: TaxCode) = tc.dutyType.ordinal * 10000
    + tc.exciseCategory.map(ec => ec.ordinal * 1000).getOrElse(0)
    + tc.value.filter(_.isDigit).toIntOption.getOrElse(0)

  implicit val ordering: Ordering[TaxCode] =
    Ordering.fromLessThan[TaxCode]((tc1, tc2) =>
      val hash1 = hash(tc1)
      val hash2 = hash(tc2)
      if hash1 != hash2
      then summon[Ordering[Int]].lt(hash1, hash2)
      else summon[Ordering[String]].lt(tc1.value, tc2.value)
    )
}
