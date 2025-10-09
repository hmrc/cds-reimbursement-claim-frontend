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

sealed trait RejectedGoodsClaimType

object RejectedGoodsClaimType extends EnumerationFormat[RejectedGoodsClaimType] {

  case object Individual extends RejectedGoodsClaimType
  case object Multiple extends RejectedGoodsClaimType
  case object Scheduled extends RejectedGoodsClaimType

  override val values: Set[RejectedGoodsClaimType] = Set(Individual, Multiple, Scheduled)

  private[models] val rejectedGoodsClaimTypeStringMap: Map[String, RejectedGoodsClaimType] =
    values.map(a => a.toString -> a).toMap

  def has(claimType: String): Boolean =
    rejectedGoodsClaimTypeStringMap.contains(claimType)

  def find(claimType: String): Option[RejectedGoodsClaimType] =
    rejectedGoodsClaimTypeStringMap.get(claimType)

  def findUnsafe(claimType: String): RejectedGoodsClaimType =
    rejectedGoodsClaimTypeStringMap(claimType)
}
