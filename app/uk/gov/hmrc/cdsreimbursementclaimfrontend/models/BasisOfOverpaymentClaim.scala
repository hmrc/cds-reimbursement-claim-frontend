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

import cats.Id
import cats.implicits.catsSyntaxOption
import cats.kernel.Eq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat
import scala.math.Ordering

sealed trait BasisOfOverpaymentClaim {
  val order: Int
}

object BasisOfOverpaymentClaim extends EnumerationFormat[BasisOfOverpaymentClaim] {

  case object DuplicateEntry extends BasisOfOverpaymentClaim { val order = 0 }
  case object DutySuspension extends BasisOfOverpaymentClaim { val order = 2 }
  case object EndUseRelief extends BasisOfOverpaymentClaim { val order = 3 }
  case object IncorrectAdditionalInformationCode extends BasisOfOverpaymentClaim {
    val order = 4
  } //Northern Ireland only
  case object IncorrectCommodityCode extends BasisOfOverpaymentClaim { val order = 5 }
  case object IncorrectCpc extends BasisOfOverpaymentClaim { val order = 6 }

  case object IncorrectEoriAndDan extends BasisOfOverpaymentClaim { val order = 7 }
  case object IncorrectExciseValue extends BasisOfOverpaymentClaim { val order = 8 } //Northern Ireland only
  case object IncorrectValue extends BasisOfOverpaymentClaim { val order = 9 }
  case object InwardProcessingReliefFromCustomsDuty extends BasisOfOverpaymentClaim { val order = 10 }
  case object OutwardProcessingRelief extends BasisOfOverpaymentClaim { val order = 11 }
  case object PersonalEffects extends BasisOfOverpaymentClaim { val order = 12 }
  case object Preference extends BasisOfOverpaymentClaim { val order = 13 }
  case object ProofOfReturnRefundGiven extends BasisOfOverpaymentClaim { val order = 14 }
  case object RGR extends BasisOfOverpaymentClaim { val order = 15 }
  case object Quota extends BasisOfOverpaymentClaim { val order = 16 }
  case object Miscellaneous extends BasisOfOverpaymentClaim { val order = 17 }

  val values: Set[BasisOfOverpaymentClaim] =
    Set(
      DuplicateEntry,
      DutySuspension,
      EndUseRelief,
      IncorrectAdditionalInformationCode,
      IncorrectCommodityCode,
      IncorrectCpc,
      IncorrectEoriAndDan,
      IncorrectExciseValue,
      IncorrectValue,
      InwardProcessingReliefFromCustomsDuty,
      OutwardProcessingRelief,
      PersonalEffects,
      Preference,
      ProofOfReturnRefundGiven,
      RGR,
      Quota,
      Miscellaneous
    )

  val northernIreland: Set[BasisOfOverpaymentClaim] = Set(
    IncorrectExciseValue,
    IncorrectAdditionalInformationCode
  )

  private val ukExciseCodeStrings: Set[String] =
    TaxCodes.excise.map(_.value).toSet

  def excludeNorthernIrelandClaims(
    hasDuplicateEntryClaim: Boolean,
    displayDeclarationOpt: Option[DisplayDeclaration],
    hasDanOption: Boolean = false
  ): Set[BasisOfOverpaymentClaim] = {

    val receivedExciseCodes: List[String] =
      displayDeclarationOpt
        .flatMap(_.displayResponseDetail.ndrcDetails.map(_.map(_.taxType)))
        .getOrElse(Nil)

    val hasNorthernIrelandExciseCodes =
      receivedExciseCodes.toSet.intersect(ukExciseCodeStrings).nonEmpty

    val baseClaims =
      if (hasDuplicateEntryClaim) values
      else values - DuplicateEntry

    val claims =
      if (hasNorthernIrelandExciseCodes) baseClaims
      else baseClaims - IncorrectExciseValue

    if (hasDanOption) claims else claims - IncorrectEoriAndDan
  }

  val validator: Validator[Id, BasisOfOverpaymentClaim] = maybeBasisOfClaim =>
    maybeBasisOfClaim.toValidNel(MissingAnswerError("Basis of claims"))

  private[models] val basisOfOverpaymentsGoodsStringMap: Map[String, BasisOfOverpaymentClaim] =
    values.map(a => a.toString -> a).toMap

  def has(basisOfOverpaymentsGoods: String): Boolean                                          =
    basisOfOverpaymentsGoodsStringMap.contains(basisOfOverpaymentsGoods)

  def find(basisOfOverpaymentsGoods: String): Option[BasisOfOverpaymentClaim] =
    basisOfOverpaymentsGoodsStringMap.get(basisOfOverpaymentsGoods)

  def findUnsafe(basisOfOverpaymentsGoods: String): BasisOfOverpaymentClaim =
    basisOfOverpaymentsGoodsStringMap(basisOfOverpaymentsGoods)

  implicit val basisOfClaimEquality: Eq[BasisOfOverpaymentClaim] =
    Eq.fromUniversalEquals[BasisOfOverpaymentClaim]

  implicit val ordering: Ordering[BasisOfOverpaymentClaim] =
    Ordering.by[BasisOfOverpaymentClaim, Int](_.order)

}
