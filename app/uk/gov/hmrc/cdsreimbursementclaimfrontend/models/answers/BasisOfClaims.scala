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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaimAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaims.all
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

import scala.collection.immutable.HashSet

final case class BasisOfClaims(items: List[BasisOfClaimAnswer]) extends AnyVal {
  def buildKey(parentKey: String, basisOfClaim: BasisOfClaimAnswer): String =
    s"$parentKey.reason.d${all.indexOf(basisOfClaim)}"
}

object BasisOfClaims {

  private val ukExciseCodeStrings: HashSet[String] =
    HashSet(TaxCodes.excise.map(_.value): _*)

  // The order of the items in this list is the order they
  // will appear on the select basis for claim page on the C285 journey.
  // Ensure that the messages starting `select-basis-for-claim.reason.d`
  // are in the required order.
  val all: List[BasisOfClaimAnswer] = List(
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

  val northernIreland: List[BasisOfClaimAnswer] = List(
    IncorrectExciseValue,
    IncorrectAdditionalInformationCode
  )

  def apply(): Builder = Builder(all)

  def withoutDuplicateEntry(): Builder =
    Builder(all.diff(DuplicateEntry :: Nil))

  final case class Builder(claims: List[BasisOfClaimAnswer]) {

    def excludeNorthernIrelandClaims(claim: DraftClaim): BasisOfClaims = {

      val isNorthernIrelandJourney =
        claim.whetherNorthernIrelandAnswer.getOrElse(No)

      val receivedExciseCodes =
        claim.displayDeclaration
          .flatMap(_.displayResponseDetail.ndrcDetails.map(_.map(_.taxType)))
          .getOrElse(Nil)

      val hasNorthernIrelandExciseCodes =
        receivedExciseCodes.toSet.intersect(ukExciseCodeStrings).nonEmpty

      val items = isNorthernIrelandJourney match {
        case No  =>
          claims.diff(northernIreland)
        case Yes =>
          if (hasNorthernIrelandExciseCodes) claims
          else claims.diff(IncorrectExciseValue :: Nil)
      }

      BasisOfClaims(items)
    }
  }

  def indexOf(basisOfClaim: BasisOfClaimAnswer): Int =
    all.indexOf(basisOfClaim)

  def contains(index: Int): Boolean =
    all.lift(index).isDefined

  implicit def basisOfClaims2List(basisOfClaims: BasisOfClaims): List[BasisOfClaimAnswer] =
    basisOfClaims.items
}
