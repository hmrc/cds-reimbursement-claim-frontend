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

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.MessagesHelper.combine
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
object CdsDisplayDeclarationSummary extends AnswerSummary[DisplayDeclaration] {

  override def render(
    declaration: DisplayDeclaration,
    key: String,
    subKey: Option[String],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = render(declaration, key, subKey)

  def render(
    declaration: DisplayDeclaration,
    key: String,
    subKey: Option[String] = None,
    showImportMrn: Boolean = true,
    showMethodOfPayment: Boolean = true
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.mrn-label"))),
          value = Value(Text(declaration.displayResponseDetail.declarationId))
        ).some.filter(_ => showImportMrn),
        declaration.getMaybeLRN match {
          case Some(lrn) =>
            SummaryListRow(
              key = Key(HtmlContent(messages(combine(key, subKey, "lrn-label")))),
              value = Value(Text(lrn))
            ).some
          case _         => None
        },
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.import-date-label"))),
          value = Value(Text(toDisplayDate(declaration.displayResponseDetail.acceptanceDate)))
        ).some,
        declaration.getMethodsOfPayment
          .map { methods =>
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.method-of-payment-label"))),
              value = Value(Text(MethodOfPaymentSummary(methods)))
            )
          }
          .filter(_ => showMethodOfPayment),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.paid-duties-charges-label"))),
          value = Value(Text(declaration.totalDutiesPaidCharges.toPoundSterlingString))
        ).some,
        declaration.totalVatPaidCharges.map(vatCharges =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.paid-vat-charges-label"))),
            value = Value(Text(vatCharges.toPoundSterlingString))
          )
        ),
        declaration.consigneeName.map { name =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-name-label"))),
            value = Value(Text(name))
          )
        },
        declaration.consigneeEmail.map { email =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-email-label"))),
            value = Value(Text(email))
          )
        },
        declaration.consigneeTelephone.map { telephone =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-telephone-label"))),
            value = Value(Text(telephone))
          )
        },
        declaration.consigneeAddress.map { address =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-address-label"))),
            value = Value(HtmlContent(address))
          )
        },
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.declarant-name-label"))),
          value = Value(Text(declaration.declarantName))
        ).some,
        declaration.declarantContactAddress.map { address =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.declarant-address-label"))),
            value = Value(HtmlContent(address))
          )
        }
      ).flatMap(_.toList)
    )
}
