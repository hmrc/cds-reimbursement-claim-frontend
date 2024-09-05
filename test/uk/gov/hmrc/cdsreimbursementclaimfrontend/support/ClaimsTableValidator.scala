/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.support

import org.jsoup.nodes.Document
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

trait ClaimsTableValidator {
  this: Matchers =>

  def validateClaimsTable(doc: Document, reimbursements: Seq[Reimbursement], claimAction: TaxCode => Call)(implicit
    m: Messages
  ) = {

    validateClaimsTableHeaders(doc)

    reimbursements.zipWithIndex.map {
      case (Reimbursement(taxCode, amount, _, paidAmount, Some(correctedAmount)), idx) =>
        doc
          .getElementById(s"selected-claim-${idx + 1}")
          .text()                                                     shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
        doc.getElementById(s"what-you-paid-${idx + 1}").text()        shouldBe paidAmount.toPoundSterlingString
        doc.getElementById(s"you-should-have-paid-${idx + 1}").text() shouldBe correctedAmount.toPoundSterlingString
        doc.getElementById(s"claim-amount-${idx + 1}").text()         shouldBe amount.toPoundSterlingString
        doc.getElementById(s"change-${idx + 1}").html()               shouldBe m(
          "check-claim.table.change-link",
          claimAction(taxCode).url,
          s"change-link-${idx + 1}"
        )
    }

  }

  private def validateClaimsTableHeaders(doc: Document)(implicit m: Messages) = {
    doc.getElementById("selected-claim-header").text()   shouldBe m("check-claim.table-header.selected-charges")
    doc.getElementById("you-paid-header").text()         shouldBe m("check-claim.table-header.you-paid")
    doc.getElementById("should-have-paid-header").text() shouldBe m("check-claim.table-header.should-have-paid")
    doc.getElementById("claim-amount-header").text()     shouldBe m("check-claim.table-header.claim-amount")
    doc.getElementById("blank-header").text()            shouldBe ""
  }
}
