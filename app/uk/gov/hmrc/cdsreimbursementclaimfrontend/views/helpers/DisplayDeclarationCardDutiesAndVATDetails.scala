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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object DisplayDeclarationCardDutiesAndVATDetails {

  def apply(
    declaration: DisplayDeclaration,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList = {
    val methodsOfPaymentRow = declaration.getMethodsOfPayment match {
      case Some(methods) if methods.nonEmpty =>
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.method-of-payment-label"))),
            value = Value(Text(MethodOfPaymentSummary(methods)))
          )
        )
      case _                                 => Seq.empty
    }
    val ndrcDutiesRows      = declaration.getNdrcDutiesWithAmount
      .map(_.map { case (taxCode, amount) =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"tax-code.${taxCode.value}"))),
          value = Value(Text(amount.toPoundSterlingString))
        )
      })
      .getOrElse(Seq.empty)
    val totalRow            = Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.total"))),
        value = Value(Text(declaration.totalPaidCharges.toPoundSterlingString))
      )
    )
    SummaryList(methodsOfPaymentRow ++ ndrcDutiesRows ++ totalRow)
  }
}
