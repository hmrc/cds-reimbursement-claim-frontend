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

import cats.data.NonEmptyList
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.{routes => overpaymentsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import play.api.mvc.Call

object ClaimSummaryHelper {

  private val key = "check-claim-summary"

  def makeClaimSummary(claims: NonEmptyList[ClaimedReimbursement])(implicit messages: Messages): List[SummaryListRow] =
    makeClaimSummaryRows(claims) ++ makeTotalRow(claims)

  def makeClaimSummary(claims: Seq[(TaxCode, BigDecimal, Call)])(implicit messages: Messages): List[SummaryListRow] =
    makeClaimSummaryRows(claims) ++ makeTotalRow(claims)

  def makeClaimSummaryRows(claims: NonEmptyList[ClaimedReimbursement])(implicit
    messages: Messages
  ): List[SummaryListRow] =
    makeClaimSummaryRows(
      claims.toList
        .map(c =>
          (
            c.taxCode,
            c.claimAmount,
            overpaymentsSingleRoutes.EnterSingleClaimController.enterClaim(c.id)
          )
        )
    )

  def makeClaimSummaryRows(claims: Seq[(TaxCode, BigDecimal, Call)])(implicit
    messages: Messages
  ): List[SummaryListRow] =
    claims.toList.zipWithIndex.map { case ((taxCode, claimAmount, changeCall), index) =>
      SummaryListRow(
        key = Key(Text(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}")),
        value = Value(Text(claimAmount.toPoundSterlingString)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${changeCall.url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  s"${OrdinalNumber.label(index + 1).capitalize} MRN: ${TaxCodes
                    .findTaxType(taxCode)} Duty ${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}"
                )
              )
            )
          )
        )
      )
    }

  def makeTotalRow(claims: NonEmptyList[ClaimedReimbursement])(implicit messages: Messages): List[SummaryListRow] =
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.total"))),
      value = Value(Text(claims.toList.map(_.claimAmount).sum.toPoundSterlingString)),
      classes = "govuk-!-margin-bottom-9"
    ) :: Nil

  def makeTotalRow(claims: Seq[(TaxCode, BigDecimal, Call)])(implicit messages: Messages): List[SummaryListRow] =
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.total"))),
      value = Value(Text(claims.map(_._2).sum.toPoundSterlingString)),
      classes = "govuk-!-margin-bottom-9"
    ) :: Nil

}