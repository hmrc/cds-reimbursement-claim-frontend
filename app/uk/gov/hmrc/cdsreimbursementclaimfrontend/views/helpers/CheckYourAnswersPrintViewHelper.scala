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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.routes as overpaymentsMultipleRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.routes as overpaymentsScheduledRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes as overpaymentsSingleRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.routes as rejectedMultipleRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.routes as rejectedScheduledRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes as rejectedSingleRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.routes as securitiesRoute
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object CheckYourAnswersPrintViewHelper {

  def render(caseNumber: String, submissionDate: LocalDateTime)(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.claim-reference"))),
          value = Value(Text(caseNumber))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.print-view.submitted"))),
          value = Value(HtmlContent(getFormattedSubmissionDate(submissionDate)))
        )
      )
    )

  def renderClaimDetails(
    caseNumber: String,
    mrn: MRN,
    amountRequested: String,
    mrnKey: String = "confirmation-of-submission.mrn",
    submissionDate: LocalDateTime
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.claim-reference"))),
          value = Value(Text(caseNumber))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(mrnKey))),
          value = Value(Text(mrn.value))
        )
      )
        ++ (if amountRequested == "£0.00" then Seq.empty
            else
              Seq(
                SummaryListRow(
                  key = Key(HtmlContent(messages("confirmation-of-submission.reimbursement-amount"))),
                  value = Value(Text(amountRequested))
                )
              )
        )
        ++ Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.print-view.submitted"))),
            value = Value(HtmlContent(getFormattedSubmissionDate(submissionDate)))
          )
        )
    )

  def renderClaimDetailsForMultiple(
    caseNumber: String,
    amountRequested: BigDecimal,
    mrns: Seq[MRN],
    submissionDate: LocalDateTime
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.claim-reference"))),
          value = Value(Text(caseNumber))
        )
      ) ++ mrns.zipWithIndex.map { case (mrn, index) =>
        SummaryListRow(
          key = Key(
            HtmlContent(OrdinalNumberMrnHelper(index + 1))
          ),
          value = Value(Text(mrn.value))
        )
      } ++ Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.reimbursement-amount"))),
          value = Value(Text(amountRequested.toPoundSterlingString))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.print-view.submitted"))),
          value = Value(HtmlContent(getFormattedSubmissionDate(submissionDate)))
        )
      )
    )

  def getPrintViewUrl(claim: ClaimBase): String = claim match {
    case _: OverpaymentsSingleClaim     => overpaymentsSingleRoute.CheckYourAnswersController.showPrintView.url
    case _: OverpaymentsMultipleClaim   => overpaymentsMultipleRoute.CheckYourAnswersController.showPrintView.url
    case _: OverpaymentsScheduledClaim  => overpaymentsScheduledRoute.CheckYourAnswersController.showPrintView.url
    case _: SecuritiesClaim             => securitiesRoute.CheckYourAnswersController.showPrintView.url
    case _: RejectedGoodsSingleClaim    => rejectedSingleRoute.CheckYourAnswersController.showPrintView.url
    case _: RejectedGoodsMultipleClaim  => rejectedMultipleRoute.CheckYourAnswersController.showPrintView.url
    case _: RejectedGoodsScheduledClaim => rejectedScheduledRoute.CheckYourAnswersController.showPrintView.url
  }

  private def getFormattedSubmissionDate(submissionDate: LocalDateTime)(implicit messages: Messages): String =
    s"${submissionDate.format(DateTimeFormatter.ofPattern("h:mm a"))}, ${messages(s"day-of-week.${submissionDate.getDayOfWeek.getValue}")}" ++
      "<br/>" ++ toDisplayDate(submissionDate.toLocalDate)

}
