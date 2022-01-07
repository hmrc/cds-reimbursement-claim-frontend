/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.LanguageHelper.lang
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

object DisplayDeclarationSummary extends RejectedGoodsAnswerSummary[DisplayDeclaration] {

  def render(key: String, declaration: DisplayDeclaration)(implicit
    subKey: Option[String],
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(Text("")),
        value = Value()
      ).some,
      SummaryListRow(
        key = Key(Text(messages(lang(key, subKey, "mrn-label")))),
        value = Value(Text(declaration.displayResponseDetail.declarationId))
      ).some,
      SummaryListRow(
        key = Key(Text(messages(s"$key.import-date-label"))),
        value = Value(Text(declaration.displayResponseDetail.acceptanceDate))
      ).some,
      SummaryListRow(
        key = Key(Text(messages(s"$key.paid-charges-label"))),
        value = Value(Text(declaration.totalPaidCharges.toPoundSterlingString))
      ).some,
      declaration.consigneeName.map { name =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.importer-name-label"))),
          value = Value(Text(name))
        )
      },
      declaration.consigneeEmail.map { email =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.importer-email-label"))),
          value = Value(Text(email))
        )
      },
      declaration.consigneeTelephone.map { telephone =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.importer-telephone-label"))),
          value = Value(Text(telephone))
        )
      },
      declaration.consigneeAddress.map { address =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.importer-address-label"))),
          value = Value(HtmlContent(address))
        )
      },
      SummaryListRow(
        key = Key(Text(messages(s"$key.declarant-name-label"))),
        value = Value(Text(declaration.declarantName))
      ).some,
      declaration.declarantContactAddress.map { address =>
        SummaryListRow(
          key = Key(Text(messages(s"$key.declarant-address-label"))),
          value = Value(HtmlContent(address))
        )
      }
    ).flatMap(_.toList)
  )

}
