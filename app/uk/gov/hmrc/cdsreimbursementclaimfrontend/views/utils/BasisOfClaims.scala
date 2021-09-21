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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim.{DuplicateEntry, IncorrectAdditionalInformationCode, IncorrectEoriAndDefermentAccountNumber, IncorrectExciseValue, allClaimsTypes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, ClaimNorthernIrelandAnswer, DraftClaim, TaxCode}

final case class BasisOfClaims(items: List[BasisOfClaim]) extends AnyVal {
  def buildKey(parentKey: String, basisOfClaim: BasisOfClaim): String =
    s"$parentKey.reason.d${basisOfClaim.value}"
}

object BasisOfClaims {

  def apply(): Builder = Builder(allClaimsTypes)

  def withoutJourneyClaimsIfApplies(journey: JourneyBindable): Builder =
    BasisOfClaims().filterUsing(journey)

  final case class Builder(claims: List[BasisOfClaim]) {

    def filterUsing(journey: JourneyBindable): Builder =
      copy(
        if (journey === JourneyBindable.Scheduled || journey === JourneyBindable.Multiple)
          claims.diff(List(DuplicateEntry, IncorrectEoriAndDefermentAccountNumber))
        else claims
      )

    def withoutNorthernIrelandClaimsIfApplies(claim: DraftClaim): BasisOfClaims = {

      val isNorthernIrelandJourney = claim
        .fold(_.claimNorthernIrelandAnswer)
        .getOrElse(ClaimNorthernIrelandAnswer.No)

      val receivedExciseCodes = claim
        .fold(_.displayDeclaration)
        .flatMap(_.displayResponseDetail.ndrcDetails.map(_.map(_.taxType)))
        .getOrElse(Nil)

      val hasNorthernIrelandExciseCodes =
        receivedExciseCodes.toSet.intersect(TaxCode.listOfUKExciseCodeStrings).nonEmpty

      val items = isNorthernIrelandJourney match {
        case ClaimNorthernIrelandAnswer.No  =>
          claims.diff(
            List(IncorrectExciseValue, IncorrectAdditionalInformationCode)
          )
        case ClaimNorthernIrelandAnswer.Yes =>
          if (hasNorthernIrelandExciseCodes) claims
          else claims.diff(List(IncorrectExciseValue))
      }

      BasisOfClaims(items)
    }
  }

  implicit def basisOfClaims2List(basisOfClaims: BasisOfClaims): List[BasisOfClaim] =
    basisOfClaims.items
}
