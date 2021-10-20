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

import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType._

object DutyTypes {

  val custom: Seq[DutyType] = List(UkDuty, EuDuty)

  val excise: Seq[DutyType] = List(
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

  val all: Seq[DutyType] = custom ++ excise

  private val dutyTypesStringMap           =
    all.map(dutyType => dutyType.repr -> dutyType).toMap

  def has(representation: String): Boolean =
    all.exists(_.repr === representation)

  def find(representation: String): Option[DutyType] =
    dutyTypesStringMap.get(representation)

  def findUnsafe(representation: String): DutyType =
    dutyTypesStringMap(representation)
}
