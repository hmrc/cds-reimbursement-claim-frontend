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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Empty
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument

object EvidenceDocumentsSummary {

  def apply(
    answers: Seq[EvidenceDocument],
    key: String,
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(if (answers.isEmpty) Empty else Text(messages(s"$key.label"))),
          value = Value(
            if (answers.isEmpty)
              Text(messages(s"$key.empty"))
            else
              HtmlContent(
                answers
                  .map(document =>
                    Paragraph(
                      document.fileName,
                      messages(s"supporting-evidence.choose-document-type.document-type.${document.documentType}")
                    ).toString
                  )
                  .toList
                  .mkString("")
              )
          ),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.hidden-label"))
                )
              )
            )
          )
        )
      )
    )

  def forSecurities(
    answers: Seq[EvidenceDocument],
    key: String,
    changeCall: Call
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      answers.map(document =>
        SummaryListRow(
          key = Key(Text(messages(s"choose-file-type.file-type.${document.documentType}"))),
          value = Value(
            HtmlContent(
              Paragraph(
                document.fileName,
                messages(s"choose-file-type.file-type.${document.documentType}")
              ).toString
            )
          ),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.label"))
                )
              )
            )
          )
        )
      )
    )
}
