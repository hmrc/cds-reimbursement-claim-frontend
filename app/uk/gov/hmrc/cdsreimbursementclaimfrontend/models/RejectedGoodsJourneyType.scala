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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait RejectedGoodsJourneyType

object RejectedGoodsJourneyType extends EnumerationFormat[RejectedGoodsJourneyType] {

  case object Individual extends RejectedGoodsJourneyType
  case object Multiple extends RejectedGoodsJourneyType
  case object Scheduled extends RejectedGoodsJourneyType

  override val values: Set[RejectedGoodsJourneyType] = Set(Individual, Multiple, Scheduled)

  private[models] val rejectedGoodsJourneyTypeStringMap: Map[String, RejectedGoodsJourneyType] =
    values.map(a => a.toString -> a).toMap

  def has(journeyType: String): Boolean                                                        =
    rejectedGoodsJourneyTypeStringMap.contains(journeyType)

  def find(journeyType: String): Option[RejectedGoodsJourneyType] =
    rejectedGoodsJourneyTypeStringMap.get(journeyType)

  def findUnsafe(journeyType: String): RejectedGoodsJourneyType =
    rejectedGoodsJourneyTypeStringMap(journeyType)
}
