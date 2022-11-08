/*
 * Copyright 2022 HM Revenue & Customs
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

sealed trait OverpaymentsJourneyType

object OverpaymentsJourneyType extends EnumerationFormat[OverpaymentsJourneyType] {

  case object Individual extends OverpaymentsJourneyType
  case object Multiple extends OverpaymentsJourneyType
  case object Scheduled extends OverpaymentsJourneyType

  override val values: Set[OverpaymentsJourneyType] = Set(Individual, Multiple, Scheduled)

  private[models] val overpaymentsJourneyTypeStringMap: Map[String, OverpaymentsJourneyType] =
    values.map(a => a.toString -> a).toMap

  def has(journeyType: String): Boolean                                                      =
    overpaymentsJourneyTypeStringMap.contains(journeyType)

  def find(journeyType: String): Option[OverpaymentsJourneyType] =
    overpaymentsJourneyTypeStringMap.get(journeyType)

  def findUnsafe(journeyType: String): OverpaymentsJourneyType =
    overpaymentsJourneyTypeStringMap(journeyType)
}
