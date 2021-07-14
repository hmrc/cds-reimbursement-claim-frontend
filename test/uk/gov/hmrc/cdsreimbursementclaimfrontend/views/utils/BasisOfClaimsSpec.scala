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

import cats.implicits.catsSyntaxOptionId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, ClaimNorthernIrelandAnswer, MovementReferenceNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.BasisOfClaims.{EntryNumberBasisOfClaims, MrnBasisOfClaims}

class BasisOfClaimsSpec extends AnyWordSpec with Matchers {

  "The basis of claims" should {

    "filter Northern Ireland claims" in {
      val draftC285Claim = sample[DraftC285Claim].copy(
        movementReferenceNumber = MovementReferenceNumber(Right(sample[MRN])).some,
        claimNorthernIrelandAnswer = ClaimNorthernIrelandAnswer.No.some
      )

      val claims = BasisOfClaims().withoutNorthernIrelandClaimsIfApplies(draftC285Claim)

      claims should be(
        MrnBasisOfClaims(items =
          List(
            DuplicateEntry,
            DutySuspension,
            EndUseRelief,
            IncorrectCommodityCode,
            IncorrectCpc,
            IncorrectValue,
            IncorrectEoriAndDefermentAccountNumber,
            InwardProcessingReliefFromCustomsDuty,
            OutwardProcessingRelief,
            PersonalEffects,
            Preference,
            RGR,
            ProofOfReturnRefundGiven,
            Miscellaneous
          )
        )
      )
    }
  }

  "contain Northern Ireland claims" in {
    val draftC285Claim = sample[DraftC285Claim].copy(
      movementReferenceNumber = MovementReferenceNumber(Left(sample[EntryNumber])).some,
      claimNorthernIrelandAnswer = ClaimNorthernIrelandAnswer.Yes.some
    )

    val claims = BasisOfClaims().withoutNorthernIrelandClaimsIfApplies(draftC285Claim)

    claims should be(
      EntryNumberBasisOfClaims(items =
        List(
          DuplicateEntry,
          DutySuspension,
          EndUseRelief,
          IncorrectCommodityCode,
          IncorrectCpc,
          IncorrectValue,
          IncorrectEoriAndDefermentAccountNumber,
          InwardProcessingReliefFromCustomsDuty,
          OutwardProcessingRelief,
          PersonalEffects,
          Preference,
          RGR,
          ProofOfReturnRefundGiven,
          CorrectionToRiskClassification,
          Miscellaneous
        )
      )
    )
  }

  "filter duplicate entry claim for scheduled journey" in {
    BasisOfClaims.withoutJourneyClaimsIfApplies(JourneyBindable.Scheduled).claims should be(
      List(
        DutySuspension,
        EndUseRelief,
        IncorrectCommodityCode,
        IncorrectCpc,
        IncorrectValue,
        IncorrectEoriAndDefermentAccountNumber,
        InwardProcessingReliefFromCustomsDuty,
        OutwardProcessingRelief,
        PersonalEffects,
        Preference,
        RGR,
        ProofOfReturnRefundGiven,
        IncorrectExciseValue,
        CorrectionToRiskClassification,
        Miscellaneous
      )
    )
  }

  "contain contain duplicate entry claim for single journey" in {
    BasisOfClaims.withoutJourneyClaimsIfApplies(JourneyBindable.Single).claims should be(
      List(
        DuplicateEntry,
        DutySuspension,
        EndUseRelief,
        IncorrectCommodityCode,
        IncorrectCpc,
        IncorrectValue,
        IncorrectEoriAndDefermentAccountNumber,
        InwardProcessingReliefFromCustomsDuty,
        OutwardProcessingRelief,
        PersonalEffects,
        Preference,
        RGR,
        ProofOfReturnRefundGiven,
        IncorrectExciseValue,
        CorrectionToRiskClassification,
        Miscellaneous
      )
    )
  }

  "build MRN basis of claims key" in {
    val basisOfClaim = sample[BasisOfClaim]

    MrnBasisOfClaims(List.empty).buildKey(parentKey = "key", basisOfClaim) should be(
      s"key.reason.d${basisOfClaim.value}"
    )
  }

  "build Entry Number basis of claims key" in {
    val basisOfClaim = sample[BasisOfClaim]

    EntryNumberBasisOfClaims(List.empty).buildKey(parentKey = "key", basisOfClaim) should be(
      s"key.reason.ern.d${basisOfClaim.value}"
    )
  }
}
