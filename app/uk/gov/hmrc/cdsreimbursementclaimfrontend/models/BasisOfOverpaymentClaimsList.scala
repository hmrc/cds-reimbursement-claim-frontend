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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList.all
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

import scala.collection.immutable.HashSet

final case class BasisOfOverpaymentClaimsList(items: List[BasisOfOverpaymentClaim]) extends AnyVal {
  def buildKey(parentKey: String, basisOfClaim: BasisOfOverpaymentClaim): String =
    s"$parentKey.reason.d${all.indexOf(basisOfClaim)}"
}

object BasisOfOverpaymentClaimsList {

  private val ukExciseCodeStrings: HashSet[String] =
    HashSet(TaxCodes.excise.map(_.value): _*)

  // The order of the items in this list is the order they
  // will appear on the select basis for claim page on the C285 journey.
  // Ensure that the messages starting `select-basis-for-claim.reason.d`
  // are in the required order.
  val all: List[BasisOfOverpaymentClaim] = List(
    DuplicateEntry,
    DutySuspension,
    EndUseRelief,
    IncorrectAdditionalInformationCode,
    IncorrectCommodityCode,
    IncorrectCpc,
    IncorrectExciseValue,
    IncorrectValue,
    InwardProcessingReliefFromCustomsDuty,
    OutwardProcessingRelief,
    PersonalEffects,
    Preference,
    ProofOfReturnRefundGiven,
    RGR,
    Miscellaneous
  )

  val northernIreland: List[BasisOfOverpaymentClaim] = List(
    IncorrectExciseValue,
    IncorrectAdditionalInformationCode
  )

  def apply(): Builder = Builder(all)

  def withoutDuplicateEntry(): Builder =
    Builder(all.diff(DuplicateEntry :: Nil))

  final case class Builder(claims: List[BasisOfOverpaymentClaim]) {

    def excludeNorthernIrelandClaims(
      isNorthernIrelandJourney: Boolean,
      displayDeclarationOpt: Option[DisplayDeclaration]
    ): BasisOfOverpaymentClaimsList = {

      val receivedExciseCodes: List[String] =
        displayDeclarationOpt
          .flatMap(_.displayResponseDetail.ndrcDetails.map(_.map(_.taxType)))
          .getOrElse(Nil)

      val hasNorthernIrelandExciseCodes =
        receivedExciseCodes.toSet.intersect(ukExciseCodeStrings).nonEmpty

      val items = isNorthernIrelandJourney match {
        case false =>
          claims.diff(northernIreland)
        case true  =>
          if (hasNorthernIrelandExciseCodes) claims
          else claims.diff(IncorrectExciseValue :: Nil)
      }

      BasisOfOverpaymentClaimsList(items)
    }
  }

  def indexOf(basisOfClaim: BasisOfOverpaymentClaim): Int =
    all.indexOf(basisOfClaim)

  def contains(index: Int): Boolean =
    all.lift(index).isDefined

  implicit def basisOfClaims2List(basisOfClaims: BasisOfOverpaymentClaimsList): List[BasisOfOverpaymentClaim] =
    basisOfClaims.items
}
