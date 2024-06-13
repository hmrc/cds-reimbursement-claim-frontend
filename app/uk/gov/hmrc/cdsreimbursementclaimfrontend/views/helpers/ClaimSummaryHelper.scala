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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement

object ClaimSummaryHelper {

  private val key = "check-claim"

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def makeClaimSummary(claims: Seq[(TaxCode, BigDecimal, Call)])(implicit messages: Messages): Seq[SummaryListRow] = {
    val reimbursements: Seq[Reimbursement] =
      claims.map { case (taxCode, amount, _) => Reimbursement(taxCode, amount, null) }

    val claimAction: TaxCode => Call       =
      claims.map { case (taxCode, _, call) => (taxCode, call) }.toMap

    makeClaimSummaryRows(reimbursements, claimAction) ++ makeTotalRow(reimbursements)
  }

  def makeClaimSummary(claims: Seq[Reimbursement], claimAction: TaxCode => Call)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    makeClaimSummaryRows(claims, claimAction) ++ makeTotalRow(claims)

  def makeClaimSummaryRows(claims: Seq[Reimbursement], claimAction: TaxCode => Call)(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    claims.zipWithIndex.map { case (Reimbursement(taxCode, claimAmount, _), index) =>
      SummaryListRow(
        key = Key(Text(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}")),
        value = Value(Text(claimAmount.toPoundSterlingString)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${claimAction(taxCode).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  s"${OrdinalNumber(index + 1).capitalize} MRN: ${TaxCodes
                    .findTaxType(taxCode)} Duty ${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}"
                )
              )
            )
          )
        )
      )
    }

  def makeTotalRow(claims: Seq[Reimbursement])(implicit messages: Messages): List[SummaryListRow] =
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.total"))),
      value = Value(Text(claims.map(_.amount).sum.toPoundSterlingString)),
      classes = "govuk-!-margin-bottom-9"
    ) :: Nil

}
