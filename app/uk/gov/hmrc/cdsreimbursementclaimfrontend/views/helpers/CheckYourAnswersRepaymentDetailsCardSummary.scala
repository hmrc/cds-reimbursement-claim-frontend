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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.routes as overpaymentsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.routes as overpaymentsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes as overpaymentsSingleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.routes as rejectedGoodsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.routes as rejectedGoodsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes as rejectedGoodsSingleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.routes as securitiesRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim

object CheckYourAnswersRepaymentDetailsCardSummary {

  def render(
    payeeTypeOpt: Option[PayeeType],
    methodOfPaymentOpt: Option[ReimbursementMethod],
    bankAccountDetailsOpt: Option[BankAccountDetails],
    changePayeeTypeCallOpt: Option[Call],
    changeMethodOfPaymentCallOpt: Option[Call],
    changeBankAccountDetailsCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      payeeTypeOpt.map { payeeType =>
        SummaryListRow(
          key = Key(Text(messages("check-your-answers.payee-type.label"))),
          value = Value(HtmlContent(messages(getPayeeTypeKey(payeeType)))),
          actions = changePayeeTypeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages("check-your-answers.payee-type.hidden"))
                )
              )
            )
          )
        )
      }.toSeq
        ++
          methodOfPaymentOpt.map { methodOfPayment =>
            SummaryListRow(
              key = Key(Text(messages("check-your-answers.repayment-method.label"))),
              value = Value(
                HtmlContent(messages(getRepaymentMethodKey("check-your-answers.repayment-method", methodOfPayment)))
              ),
              actions = changeMethodOfPaymentCallOpt.map(changeCall =>
                Actions(
                  items = Seq(
                    ActionItem(
                      href = changeCall.url,
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages("check-your-answers.repayment-method.hidden"))
                    )
                  )
                )
              )
            )
          }.toSeq ++
          bankAccountDetailsOpt.map { bankAccountDetails =>
            SummaryListRow(
              key = Key(HtmlContent(messages("check-your-answers.bank-details.label"))),
              value = Value(
                HtmlContent(
                  Seq(
                    Paragraph(
                      bankAccountDetails.accountName.value
                    ),
                    Paragraph(
                      bankAccountDetails.sortCode.value
                    ),
                    Paragraph(
                      bankAccountDetails.accountNumber.value
                    )
                  ).mkString("")
                )
              ),
              actions = changeBankAccountDetailsCallOpt.map(changeCall =>
                Actions(
                  items = Seq(
                    ActionItem(
                      href = changeCall.url,
                      content = Text(messages("cya.change")),
                      visuallyHiddenText = Some(messages("check-your-answers.bank-details.hidden"))
                    )
                  )
                )
              )
            )
          }
    )

  private def getPayeeTypeKey(payeeType: PayeeType): String = payeeType match {
    case PayeeType.Consignee      => "choose-payee-type.radio.importer"
    case PayeeType.Declarant      => "choose-payee-type.radio.declarant"
    case PayeeType.Representative => "choose-payee-type.radio.representative"
  }

  private def getRepaymentMethodKey(key: String, answer: ReimbursementMethod): String = answer match {
    case CurrentMonthAdjustment => s"$key.cma"
    case BankAccountTransfer    => s"$key.bt"
  }

  def renderForSingle(
    claim: OverpaymentsSingleClaim.Output,
    showMethodOfPaymentChangeCall: Boolean,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      Some(claim.displayPayeeType),
      Some(claim.reimbursementMethod),
      claim.bankAccountDetails,
      if !isPrintView then Some(overpaymentsSingleRoutes.ChoosePayeeTypeController.show) else None,
      if showMethodOfPaymentChangeCall && !isPrintView then
        Some(overpaymentsSingleRoutes.ChooseRepaymentMethodController.show)
      else None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(overpaymentsSingleRoutes.EnterBankAccountDetailsController.show)
      else None
    )

  def renderForSingle(
    claim: RejectedGoodsSingleClaim.Output,
    showMethodOfPaymentChangeCall: Boolean,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      Some(claim.displayPayeeType),
      Some(claim.reimbursementMethod),
      claim.bankAccountDetails,
      if !isPrintView then Some(rejectedGoodsSingleRoutes.ChoosePayeeTypeController.show) else None,
      if showMethodOfPaymentChangeCall && !isPrintView then
        Some(rejectedGoodsSingleRoutes.ChooseRepaymentMethodController.show)
      else None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(rejectedGoodsSingleRoutes.EnterBankAccountDetailsController.show)
      else None
    )

  def renderForMultiple(
    claim: OverpaymentsMultipleClaim.Output,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      Some(claim.displayPayeeType),
      None,
      claim.bankAccountDetails,
      if !isPrintView then Some(overpaymentsMultipleRoutes.ChoosePayeeTypeController.show) else None,
      None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(overpaymentsMultipleRoutes.EnterBankAccountDetailsController.show)
      else None
    )

  def renderForMultiple(
    claim: RejectedGoodsMultipleClaim.Output,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      Some(claim.displayPayeeType),
      None,
      claim.bankAccountDetails,
      if !isPrintView then Some(rejectedGoodsMultipleRoutes.ChoosePayeeTypeController.show) else None,
      None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(rejectedGoodsMultipleRoutes.EnterBankAccountDetailsController.show)
      else None
    )

  def renderForScheduled(
    claim: OverpaymentsScheduledClaim.Output,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      Some(claim.displayPayeeType),
      None,
      claim.bankAccountDetails,
      if !isPrintView then Some(overpaymentsScheduledRoutes.ChoosePayeeTypeController.show) else None,
      None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(overpaymentsScheduledRoutes.EnterBankAccountDetailsController.show)
      else None
    )

  def renderForScheduled(
    claim: RejectedGoodsScheduledClaim.Output,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      Some(claim.displayPayeeType),
      None,
      claim.bankAccountDetails,
      if !isPrintView then Some(rejectedGoodsScheduledRoutes.ChoosePayeeTypeController.show) else None,
      None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(rejectedGoodsScheduledRoutes.EnterBankAccountDetailsController.show)
      else None
    )

  def renderForSecurities(
    claim: SecuritiesClaim.Output,
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    render(
      claim.payeeType,
      None,
      claim.bankAccountDetails,
      if !isPrintView then Some(securitiesRoutes.ChoosePayeeTypeController.show) else None,
      None,
      if claim.bankAccountDetails.isDefined && !isPrintView then
        Some(securitiesRoutes.EnterBankAccountDetailsController.show)
      else None
    )
}
