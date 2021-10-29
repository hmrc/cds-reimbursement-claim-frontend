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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode._

import scala.collection.immutable.SortedMap

class SelectedDutyTaxCodesReimbursementAnswerSpec extends AnyWordSpec with Matchers {
  import SelectedDutyTaxCodesReimbursementAnswer._

  "Selected reimbursement duties and tax codes" should {
    "be ordered by positioning" in {

      val answer = SelectedDutyTaxCodesReimbursementAnswer(
        value = SortedMap(
          Beer     -> SortedMap(
            NI444 -> Reimbursement.unclaimed,
            NI440 -> Reimbursement.unclaimed
          ),
          EuDuty   -> SortedMap(
            A90 -> Reimbursement.unclaimed,
            A80 -> Reimbursement.unclaimed
          ),
          MadeWine -> SortedMap(
            NI423 -> Reimbursement.unclaimed,
            NI422 -> Reimbursement.unclaimed
          ),
          UkDuty   -> SortedMap(
            B00 -> Reimbursement.unclaimed,
            A00 -> Reimbursement.unclaimed
          )
        )
      )

      answer should be(
        SelectedDutyTaxCodesReimbursementAnswer(
          value = SortedMap(
            UkDuty   -> SortedMap(
              A00 -> Reimbursement.unclaimed,
              B00 -> Reimbursement.unclaimed
            ),
            EuDuty   -> SortedMap(
              A80 -> Reimbursement.unclaimed,
              A90 -> Reimbursement.unclaimed
            ),
            Beer     -> SortedMap(
              NI440 -> Reimbursement.unclaimed,
              NI444 -> Reimbursement.unclaimed
            ),
            MadeWine -> SortedMap(
              NI422 -> Reimbursement.unclaimed,
              NI423 -> Reimbursement.unclaimed
            )
          )
        )
      )
    }
  }
}
