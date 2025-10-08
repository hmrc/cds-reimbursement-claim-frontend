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

sealed trait OverpaymentsClaimType

object OverpaymentsClaimType extends EnumerationFormat[OverpaymentsClaimType] {

  case object Individual extends OverpaymentsClaimType
  case object Multiple extends OverpaymentsClaimType
  case object Scheduled extends OverpaymentsClaimType

  override val values: Set[OverpaymentsClaimType] = Set(Individual, Multiple, Scheduled)

  private[models] val overpaymentsClaimTypeStringMap: Map[String, OverpaymentsClaimType] =
    values.map(a => a.toString -> a).toMap

  def has(claimType: String): Boolean =
    overpaymentsClaimTypeStringMap.contains(claimType)

  def find(claimType: String): Option[OverpaymentsClaimType] =
    overpaymentsClaimTypeStringMap.get(claimType)

  def findUnsafe(claimType: String): OverpaymentsClaimType =
    overpaymentsClaimTypeStringMap(claimType)
}
