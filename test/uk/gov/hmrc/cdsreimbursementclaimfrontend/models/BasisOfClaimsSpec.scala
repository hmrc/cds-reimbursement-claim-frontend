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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class BasisOfClaimsSpec extends AnyWordSpec with Matchers {

  "The basis of claims" should {

    "contain Northern Ireland claims" in {
      val draftC285Claim =
        OverpaymentsSingleClaimGenerators.completeClaimGen.sample
          .getOrElse(OverpaymentsSingleClaimGenerators.emptyClaim)
          .answers
          .copy(
            movementReferenceNumber = Some(sample[MRN]),
            importDeclaration = None
          )

      val claims: Set[BasisOfOverpaymentClaim] =
        BasisOfOverpaymentClaim.excludeNorthernIrelandClaims(
          true,
          draftC285Claim.importDeclaration
        )

      claims should be(
        Set[BasisOfOverpaymentClaim](
          DuplicateEntry,
          DutySuspension,
          EndUseRelief,
          IncorrectAdditionalInformationCode,
          IncorrectCommodityCode,
          IncorrectCpc,
          IncorrectValue,
          InwardProcessingReliefFromCustomsDuty,
          OutwardProcessingRelief,
          PersonalEffects,
          Preference,
          IncorrectEoriAndDan,
          ProofOfReturnRefundGiven,
          RGR,
          Quota,
          Miscellaneous
        )
      )
    }

    "filter DuplicateEntry basis of claim" in {
      (BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry) should be(
        Set[BasisOfOverpaymentClaim](
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
          IncorrectEoriAndDan,
          ProofOfReturnRefundGiven,
          RGR,
          Quota,
          Miscellaneous
        )
      )
    }

    "contain DuplicateEntry basis of claim" in {
      BasisOfOverpaymentClaim.values should be(
        Set[BasisOfOverpaymentClaim](
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
          IncorrectEoriAndDan,
          ProofOfReturnRefundGiven,
          RGR,
          Quota,
          Miscellaneous
        )
      )
    }
  }
}
