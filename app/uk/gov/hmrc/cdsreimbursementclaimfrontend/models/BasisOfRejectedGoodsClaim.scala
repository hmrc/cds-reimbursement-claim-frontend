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

sealed trait BasisOfRejectedGoodsClaim

object BasisOfRejectedGoodsClaim extends EnumerationFormat[BasisOfRejectedGoodsClaim] {

  case object DamagedBeforeClearance extends BasisOfRejectedGoodsClaim
  case object Defective extends BasisOfRejectedGoodsClaim
  case object NotInAccordanceWithContract extends BasisOfRejectedGoodsClaim
  case object SpecialCircumstances extends BasisOfRejectedGoodsClaim

  override val values: Set[BasisOfRejectedGoodsClaim] =
    Set(DamagedBeforeClearance, Defective, NotInAccordanceWithContract, SpecialCircumstances)

  val allButSpecialCircumstances: Set[BasisOfRejectedGoodsClaim] =
    values - SpecialCircumstances

  private[models] val basisOfRejectedGoodsStringMap: Map[String, BasisOfRejectedGoodsClaim] =
    values.map(a => a.toString -> a).toMap

  def has(basisOfRejectedGoods: String): Boolean                                            =
    basisOfRejectedGoodsStringMap.contains(basisOfRejectedGoods)

  def find(basisOfRejectedGoods: String): Option[BasisOfRejectedGoodsClaim] =
    basisOfRejectedGoodsStringMap.get(basisOfRejectedGoods)

  def findUnsafe(basisOfRejectedGoods: String): BasisOfRejectedGoodsClaim =
    basisOfRejectedGoodsStringMap(basisOfRejectedGoods)
}
