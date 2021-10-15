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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementClaimGen._

class ReimbursementClaimAnswerSpec extends AnyWordSpec with Matchers {

  "Reimbursement Claim Answer Ops" must {

    "update the reimbursement claim answer" when {

      "a duty type have been deleted" in {
        val reimbursementClaim = sample[ReimbursementClaim]

        val dutyCodesAnswer = DutyCodesAnswer(
          Map[DutyType, List[TaxCode]](
            DutyType.UkDuty -> List(TaxCode.A00),
            DutyType.EuDuty -> List(TaxCode.A50)
          )
        )

        val reimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim
            ),
            DutyType.Beer   -> Map[TaxCode, ReimbursementClaim](
              TaxCode.NI440 -> reimbursementClaim
            )
          )
        )

        val updatedReimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim
            )
          )
        )

        reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer) shouldBe updatedReimbursementClaimAnswer

      }

      "a new duty type have been added" in {
        val reimbursementClaim = sample[ReimbursementClaim]

        val dutyCodesAnswer = DutyCodesAnswer(
          Map[DutyType, List[TaxCode]](
            DutyType.UkDuty -> List(TaxCode.A00),
            DutyType.EuDuty -> List(TaxCode.A50),
            DutyType.Beer   -> List(TaxCode.NI440)
          )
        )

        val reimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim
            )
          )
        )

        val updatedReimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim
            ),
            DutyType.Beer   -> Map[TaxCode, ReimbursementClaim](
              TaxCode.NI440 -> ReimbursementClaim.none
            )
          )
        )

        reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer) shouldBe updatedReimbursementClaimAnswer
      }

      "a tax code for a duty type has been deleted" in {
        val reimbursementClaim = sample[ReimbursementClaim]

        val dutyCodesAnswer = DutyCodesAnswer(
          Map[DutyType, List[TaxCode]](
            DutyType.UkDuty -> List(TaxCode.A00),
            DutyType.EuDuty -> List(TaxCode.A50)
          )
        )

        val reimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim,
              TaxCode.A90 -> reimbursementClaim
            )
          )
        )

        val updatedReimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim
            )
          )
        )

        reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer) shouldBe updatedReimbursementClaimAnswer

      }

      "a tax code for a duty type has been added" in {
        val reimbursementClaim = sample[ReimbursementClaim]

        val dutyCodesAnswer = DutyCodesAnswer(
          Map[DutyType, List[TaxCode]](
            DutyType.UkDuty -> List(TaxCode.A00),
            DutyType.EuDuty -> List(TaxCode.A50, TaxCode.A90)
          )
        )

        val reimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim
            )
          )
        )

        val updatedReimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim,
              TaxCode.A90 -> ReimbursementClaim.none
            )
          )
        )

        reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer) shouldBe updatedReimbursementClaimAnswer
      }

    }

    "return the same answer" when {

      "the duty type and respective duty code answer has not changed" in {
        val reimbursementClaim = sample[ReimbursementClaim]

        val dutyCodesAnswer = DutyCodesAnswer(
          Map[DutyType, List[TaxCode]](
            DutyType.UkDuty -> List(TaxCode.A00),
            DutyType.EuDuty -> List(TaxCode.A50, TaxCode.A90)
          )
        )

        val reimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim,
              TaxCode.A90 -> reimbursementClaim
            )
          )
        )

        val updatedReimbursementClaimAnswer = ReimbursementClaimAnswer(
          Map[DutyType, Map[TaxCode, ReimbursementClaim]](
            DutyType.UkDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A00 -> reimbursementClaim
            ),
            DutyType.EuDuty -> Map[TaxCode, ReimbursementClaim](
              TaxCode.A50 -> reimbursementClaim,
              TaxCode.A90 -> reimbursementClaim
            )
          )
        )

        reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer) shouldBe updatedReimbursementClaimAnswer
      }
    }
  }
}
