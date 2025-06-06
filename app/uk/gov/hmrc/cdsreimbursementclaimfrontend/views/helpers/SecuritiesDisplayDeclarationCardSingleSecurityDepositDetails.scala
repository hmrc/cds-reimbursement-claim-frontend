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

object SecuritiesDisplayDeclarationCardSingleSecurityDepositDetails {

  def apply(
    declaration: DisplayDeclaration,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    declaration.getSingleSecurityDetails
      .map { securityDetails =>
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.method-of-payment-label"))),
            value = Value(Text(messages(s"method-of-payment.${securityDetails.paymentMethod}")))
          )
        )
          ++ securityDetails.taxDetails.map { taxDetails =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"tax-code.${taxDetails.getTaxCode}"))),
              value = Value(Text(taxDetails.getAmount.toPoundSterlingString))
            )
          } ++
          Seq(
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.card.total-deposit"))),
              value = Value(Text(securityDetails.getTotalAmount.toPoundSterlingString))
            )
          )
      }
      .getOrElse(Nil)
  )
}
