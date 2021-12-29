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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.implicits.catsSyntaxOptionId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaimAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class BasisOfClaimsSpec extends AnyWordSpec with Matchers {

  "The basis of claims" should {

    "filter Northern Ireland claims" in {
      val draftC285Claim = sample[DraftClaim].copy(
        movementReferenceNumber = Some(sample[MRN]),
        whetherNorthernIrelandAnswer = No.some
      )

      val claims = BasisOfClaims().excludeNorthernIrelandClaims(draftC285Claim)

      claims should be(
        BasisOfClaims(items =
          List(
            DuplicateEntry,
            DutySuspension,
            EndUseRelief,
            IncorrectCommodityCode,
            IncorrectCpc,
            IncorrectValue,
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
    val draftC285Claim = sample[DraftClaim].copy(
      movementReferenceNumber = sample[MRN].some,
      whetherNorthernIrelandAnswer = Yes.some,
      displayDeclaration = None
    )

    val claims = BasisOfClaims().excludeNorthernIrelandClaims(draftC285Claim)

    claims should be(
      BasisOfClaims(items =
        List(
          DuplicateEntry,
          DutySuspension,
          EndUseRelief,
          IncorrectCommodityCode,
          IncorrectCpc,
          IncorrectValue,
          InwardProcessingReliefFromCustomsDuty,
          OutwardProcessingRelief,
          PersonalEffects,
          Preference,
          RGR,
          ProofOfReturnRefundGiven,
          IncorrectAdditionalInformationCode,
          Miscellaneous
        )
      )
    )
  }

  "filter duplicate entry claim and incorrect EORI for scheduled journey" in {
    BasisOfClaims.excludeNonJourneyClaims(JourneyBindable.Scheduled).claims should be(
      List(
        DutySuspension,
        EndUseRelief,
        IncorrectCommodityCode,
        IncorrectCpc,
        IncorrectValue,
        InwardProcessingReliefFromCustomsDuty,
        OutwardProcessingRelief,
        PersonalEffects,
        Preference,
        RGR,
        ProofOfReturnRefundGiven,
        IncorrectExciseValue,
        IncorrectAdditionalInformationCode,
        Miscellaneous
      )
    )
  }

  "filter duplicate entry claim and incorrect EORI for multiple journey" in {
    BasisOfClaims.excludeNonJourneyClaims(JourneyBindable.Multiple).claims should be(
      List(
        DutySuspension,
        EndUseRelief,
        IncorrectCommodityCode,
        IncorrectCpc,
        IncorrectValue,
        InwardProcessingReliefFromCustomsDuty,
        OutwardProcessingRelief,
        PersonalEffects,
        Preference,
        RGR,
        ProofOfReturnRefundGiven,
        IncorrectExciseValue,
        IncorrectAdditionalInformationCode,
        Miscellaneous
      )
    )
  }

  "contain duplicate entry and incorrect EORI claim for single journey" in {
    BasisOfClaims.excludeNonJourneyClaims(JourneyBindable.Single).claims should be(
      List(
        DuplicateEntry,
        DutySuspension,
        EndUseRelief,
        IncorrectCommodityCode,
        IncorrectCpc,
        IncorrectValue,
        InwardProcessingReliefFromCustomsDuty,
        OutwardProcessingRelief,
        PersonalEffects,
        Preference,
        RGR,
        ProofOfReturnRefundGiven,
        IncorrectExciseValue,
        IncorrectAdditionalInformationCode,
        Miscellaneous
      )
    )
  }

  "build basis of claims key" in {
    val basisOfClaim = sample[BasisOfClaimAnswer]

    BasisOfClaims(List.empty).buildKey(parentKey = "key", basisOfClaim) should be(
      s"key.reason.d${BasisOfClaims.indexOf(basisOfClaim)}"
    )
  }
}
