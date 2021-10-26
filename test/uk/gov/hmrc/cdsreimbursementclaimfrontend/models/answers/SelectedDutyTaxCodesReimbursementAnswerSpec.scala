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
            NI444 -> Reimbursement.blank,
            NI440 -> Reimbursement.blank
          ),
          EuDuty   -> SortedMap(
            A90 -> Reimbursement.blank,
            A80 -> Reimbursement.blank
          ),
          MadeWine -> SortedMap(
            NI423 -> Reimbursement.blank,
            NI422 -> Reimbursement.blank
          ),
          UkDuty   -> SortedMap(
            B00 -> Reimbursement.blank,
            A00 -> Reimbursement.blank
          )
        )
      )

      answer should be(
        SelectedDutyTaxCodesReimbursementAnswer(
          value = SortedMap(
            UkDuty   -> SortedMap(
              A00 -> Reimbursement.blank,
              B00 -> Reimbursement.blank
            ),
            EuDuty   -> SortedMap(
              A80 -> Reimbursement.blank,
              A90 -> Reimbursement.blank
            ),
            Beer     -> SortedMap(
              NI440 -> Reimbursement.blank,
              NI444 -> Reimbursement.blank
            ),
            MadeWine -> SortedMap(
              NI422 -> Reimbursement.blank,
              NI423 -> Reimbursement.blank
            )
          )
        )
      )
    }
  }
}
