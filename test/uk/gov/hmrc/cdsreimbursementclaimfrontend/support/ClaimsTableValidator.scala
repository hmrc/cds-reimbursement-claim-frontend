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
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReclaimWithAmounts
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementWithCorrectAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import scala.collection.immutable

trait ClaimsTableValidator {
  this: Matchers =>

  def validateClaimsTableForSingle(
    doc: Document,
    reimbursements: Seq[ReimbursementWithCorrectAmount],
    claimAction: TaxCode => Call
  )(implicit
    m: Messages
  ): Seq[Assertion] = {

    validateClaimsTableHeaders(doc)

    reimbursements.map { case ReimbursementWithCorrectAmount(taxCode, amount, paidAmount, _, _) =>
      doc
        .getElementById(s"selected-claim-$taxCode")
        .text()                                           shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
      doc.getElementById(s"full-amount-$taxCode").text()  shouldBe paidAmount.toPoundSterlingString
      doc.getElementById(s"claim-amount-$taxCode").text() shouldBe amount.toPoundSterlingString
      doc.getElementById(s"change-$taxCode").html()       shouldBe m(
        "check-claim.table.change-link",
        claimAction(taxCode).url,
        s"change-link-$taxCode"
      )
    }

  }

  private def validateRowsForMultiple(
    doc: Document,
    mrn: MRN,
    index: Int,
    reimbursements: Seq[ReimbursementWithCorrectAmount],
    claimAction: (Int, TaxCode) => Call
  )(implicit
    m: Messages
  ) =
    reimbursements.map { case ReimbursementWithCorrectAmount(taxCode, amount, paidAmount, _, _) =>
      val suffix = s"${mrn.value}-$taxCode"

      doc
        .getElementById(s"selected-claim-$suffix")
        .text()                                          shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
      doc.getElementById(s"full-amount-$suffix").text()  shouldBe paidAmount.toPoundSterlingString
      doc.getElementById(s"claim-amount-$suffix").text() shouldBe amount.toPoundSterlingString
      doc.getElementById(s"change-$suffix").html()       shouldBe m(
        "check-claim.table.change-link",
        claimAction(index, taxCode).url,
        s"change-link-$suffix"
      )
    }

  def validateClaimsTablesForMultiple(
    doc: Document,
    reimbursements: Seq[(MRN, Int, Seq[ReimbursementWithCorrectAmount])],
    claimAction: (Int, TaxCode) => Call
  )(implicit
    m: Messages
  ): Seq[Assertion] =
    reimbursements.map { case (mrn, index, claims) =>
      validateClaimsTableHeaders(doc, s"-${mrn.value}")
      validateRowsForMultiple(doc, mrn, index, claims, claimAction)
      validateDutyTotalRow(doc, claims, s"${mrn.value}")
    }

  private def validateRowsForScheduled(
    doc: Document,
    reimbursements: Seq[ReimbursementWithCorrectAmount],
    claimAction: (DutyType, TaxCode) => Call
  )(implicit
    m: Messages
  ) =
    reimbursements.map {
      case ReimbursementWithCorrectAmount(taxCode, amount, paidAmount, _, Some(dutyType)) =>
        val suffix = s"$dutyType-$taxCode"

        doc
          .getElementById(s"selected-claim-$suffix")
          .text()                                          shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
        doc.getElementById(s"full-amount-$suffix").text()  shouldBe paidAmount.toPoundSterlingString
        doc.getElementById(s"claim-amount-$suffix").text() shouldBe amount.toPoundSterlingString
        doc.getElementById(s"change-$suffix").html()       shouldBe m(
          "check-claim.table.change-link",
          claimAction(dutyType, taxCode).url,
          s"change-link-$suffix"
        )
      case ReimbursementWithCorrectAmount(taxCode, amount, paidAmount, _, None)           =>
        val suffix = s"$taxCode"
        doc
          .getElementById(s"selected-claim-$suffix")
          .text()                                          shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
        doc.getElementById(s"full-amount-$suffix").text()  shouldBe paidAmount.toPoundSterlingString
        doc.getElementById(s"claim-amount-$suffix").text() shouldBe amount.toPoundSterlingString
    }

  def validateClaimsTablesForScheduled(
    doc: Document,
    reimbursements: Map[String, List[ReimbursementWithCorrectAmount]],
    claimAction: (DutyType, TaxCode) => Call
  )(implicit
    m: Messages
  ): immutable.Iterable[Assertion] =
    reimbursements.map { claims =>
      validateClaimsTableHeaders(doc, s"-${claims._1}")
      validateRowsForScheduled(doc, claims._2, claimAction)
      validateDutyTotalRow(doc, claims._2, s"${claims._1}")
    }

  private def validateRowsForSecurities(
    doc: Document,
    securityDepositId: String,
    reimbursements: List[ReclaimWithAmounts],
    claimAction: (String, TaxCode) => Call
  )(implicit
    m: Messages
  ) =
    reimbursements.map { case ReclaimWithAmounts(taxCode, claimAmount, paidAmount) =>
      val suffix = s"$securityDepositId-$taxCode"

      doc
        .getElementById(s"selected-claim-$suffix")
        .text()                                          shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
      doc.getElementById(s"full-amount-$suffix").text()  shouldBe paidAmount.toPoundSterlingString
      doc.getElementById(s"claim-amount-$suffix").text() shouldBe claimAmount.toPoundSterlingString
      doc.getElementById(s"change-$suffix").html()       shouldBe m(
        "check-claim.table.change-link",
        claimAction(securityDepositId, taxCode).url,
        s"change-link-$suffix"
      )
    }

  private def validateRowsForSingleSecurity(
    doc: Document,
    securityDepositId: String,
    reimbursements: List[ReclaimWithAmounts],
    claimAction: (String, TaxCode) => Call
  )(implicit
    m: Messages
  ) =
    reimbursements.map { case ReclaimWithAmounts(taxCode, claimAmount, paidAmount) =>
      val suffix = taxCode.value

      doc
        .getElementById(s"selected-claim-$suffix")
        .text()                                               shouldBe s"$taxCode - ${m(s"select-duties.duty.$taxCode")}"
      doc.getElementById(s"full-amount-$suffix").text()       shouldBe paidAmount.toPoundSterlingString
      doc.getElementById(s"claim-amount-$suffix").text()      shouldBe claimAmount.toPoundSterlingString
      doc.getElementById(s"change-$suffix").text()            shouldBe "Change" + m(
        "check-claim.securities.single.hidden.duty-amount",
        m(s"select-duties.duty.${taxCode.value}")
      )
      doc.getElementById(s"change-link-$suffix").attr("href") shouldBe claimAction(securityDepositId, taxCode).url
    }

  private def validateTotalRow(doc: Document, claims: Seq[ReclaimWithAmounts], suffix: String)(implicit
    m: Messages
  ) = {
    doc.getElementById(s"total-$suffix").text()      shouldBe m("check-claim.total.header")
    doc
      .getElementById(s"full-amount-total-$suffix")
      .text()                                        shouldBe claims.map(_.paidAmount).sum.toPoundSterlingString
    doc
      .getElementById(s"claim-amount-total-$suffix")
      .text()                                        shouldBe claims.map(_.claimAmount).sum.toPoundSterlingString
    doc.getElementById(s"blank-cell-$suffix").text() shouldBe ""
  }

  def validateClaimsTablesForSecurities(
    doc: Document,
    reimbursements: Map[String, List[ReclaimWithAmounts]],
    claimAction: (String, TaxCode) => Call
  )(implicit
    m: Messages
  ): immutable.Iterable[Assertion] =
    reimbursements.map { case (securityDepositId, reclaimsList) =>
      validateClaimsTableHeaders(doc, s"-$securityDepositId")
      validateRowsForSecurities(doc, securityDepositId, reclaimsList, claimAction)
      validateTotalRow(doc, reclaimsList, securityDepositId)
    }

  def validateClaimsTablesForSingleSecurities(
    doc: Document,
    securityDepositId: String,
    reclaims: List[ReclaimWithAmounts],
    claimAction: (String, TaxCode) => Call
  )(implicit
    m: Messages
  ): immutable.Iterable[Assertion] = {
    validateClaimsTableSingleSecurityHeaders(doc)
    validateRowsForSingleSecurity(doc, securityDepositId, reclaims, claimAction)
  }

  def toReimbursementWithCorrectAmount(
    reimbursements: Seq[Reimbursement]
  ): Seq[ReimbursementWithCorrectAmount] =
    reimbursements.map { reimbursement =>
      ReimbursementWithCorrectAmount(
        reimbursement.taxCode,
        reimbursement.amount,
        reimbursement.paidAmount,
        reimbursement.correctedAmount.getOrElse(BigDecimal(0))
      )
    }

  private def validateClaimsTableHeaders(doc: Document, suffix: String = "")(implicit m: Messages) = {
    doc.getElementById(s"selected-claim-header$suffix").text() shouldBe m("check-claim.table-header.selected-charges")
    doc.getElementById(s"full-amount-header$suffix").text()    shouldBe m("check-claim.table-header.full-amount")
    doc.getElementById(s"claim-amount-header$suffix").text()   shouldBe m("check-claim.table-header.claim-amount")
    doc.getElementById(s"blank-header$suffix").text()          shouldBe ""
  }

  private def validateClaimsTableSingleSecurityHeaders(doc: Document)(implicit m: Messages) = {
    doc.getElementById(s"selected-claim-header").text() shouldBe m("check-claim.table-header.selected-charges")
    doc.getElementById(s"full-amount-header").text()    shouldBe m("check-claim.table-header.full-amount")
    doc.getElementById(s"claim-amount-header").text()   shouldBe m("check-claim.table-header.claim-amount")
    doc.getElementById(s"blank-header").text()          shouldBe ""
  }

  private def validateDutyTotalRow(doc: Document, claims: Seq[ReimbursementWithCorrectAmount], suffix: String = "")(
    implicit m: Messages
  ) = {
    doc.getElementById(s"total-$suffix").text()              shouldBe m("check-claim.total.header")
    doc
      .getElementById(s"full-amount-total-$suffix")
      .text()                                                shouldBe claims.map(_.paidAmount).sum.toPoundSterlingString
    doc.getElementById(s"claim-amount-total-$suffix").text() shouldBe claims.map(_.amount).sum.toPoundSterlingString
    doc.getElementById(s"blank-cell-$suffix").text()         shouldBe ""
  }
}
