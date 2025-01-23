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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object DisplayDeclarationCardImporterDetails {

  def apply(
    declaration: DisplayDeclaration,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
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
      declaration.consigneeAddress.map { address =>
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.importer-address-label"))),
          value = Value(HtmlContent(address))
        )
      }
    ).flatMap(_.toSeq)
  )
}
