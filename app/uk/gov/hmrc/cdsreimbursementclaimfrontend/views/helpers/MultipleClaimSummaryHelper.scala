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
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object MultipleClaimSummaryHelper {

  private val key = "check-claim.multiple"

  def makeClaimSummaryRows(
    mrnIndex: Int,
    claims: Map[TaxCode, BigDecimal],
    changeCall: (Int, TaxCode) => Call
  )(implicit messages: Messages): List[SummaryListRow] =
    claims.toList.map { case (taxCode, reimbursementAmount) =>
      SummaryListRow(
        key = Key(Text(s"$taxCode - ${messages(s"select-duties.duty.$taxCode")}")),
        value = Value(Text(reimbursementAmount.toPoundSterlingString)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${changeCall(mrnIndex, taxCode).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"select-duties.duty.$taxCode"))
              )
            )
          )
        )
      )
    } ++ makeTotalRow(claims)

  def makeTotalRow(
    claims: Map[TaxCode, BigDecimal]
  )(implicit messages: Messages): List[SummaryListRow] = {
    val total = claims.values.sum
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.total"))),
      value = Value(Text(total.toPoundSterlingString))
    ) :: Nil
  }

  def makeOverallTotalRow(
    claims: Seq[(MRN, Int, Map[TaxCode, BigDecimal])]
  )(implicit messages: Messages): SummaryListRow = {
    val overallTotal: BigDecimal =
      claims.map(_._3.values.sum).sum
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.overall-total.label"))),
      value = Value(Text(overallTotal.toPoundSterlingString))
    )
  }

  def makeClaimSummaryRows(
    mrnIndex: Int,
    claims: NonEmptyList[ClaimedReimbursement],
    changeCall: (Int, TaxCode) => Call
  )(implicit messages: Messages): List[SummaryListRow] =
    (claims.map { claim =>
      SummaryListRow(
        key = Key(Text(s"${claim.taxCode} - ${messages(s"select-duties.duty.${claim.taxCode}")}")),
        value = Value(Text(claim.claimAmount.toPoundSterlingString)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${changeCall(mrnIndex, claim.taxCode).url}",
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"select-duties.duty.${claim.taxCode}"))
              )
            )
          )
        )
      )
    } ++ makeTotalRow(claims)).toList

  def makeTotalRow(
    claims: NonEmptyList[ClaimedReimbursement]
  )(implicit messages: Messages): List[SummaryListRow] = {
    val total = claims.map(_.claimAmount).toList.sum
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.total"))),
      value = Value(Text(total.toPoundSterlingString))
    ) :: Nil
  }

  def makeOverallTotalRow(
    mrnWithClaimsList: NonEmptyList[(Int, MRN, NonEmptyList[ClaimedReimbursement])]
  )(implicit messages: Messages): SummaryListRow = {
    val overallTotal: BigDecimal =
      mrnWithClaimsList.flatMap(_._3.map(_.claimAmount)).toList.sum
    SummaryListRow(
      key = Key(HtmlContent(messages(s"$key.overall-total.label"))),
      value = Value(Text(overallTotal.toPoundSterlingString))
    )
  }

}
