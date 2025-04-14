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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph
import uk.gov.hmrc.govukfrontend.views.Aliases.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType

object ProofOfOriginDocumentSummary {

  def apply(
    documents: Seq[EvidenceDocument],
    key: String,
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    val ds = documents.filter(f => f.documentType == UploadDocumentType.ProofOfOrigin)
    if ds.isEmpty then SummaryList()
    else
      SummaryList(
        Seq(
          SummaryListRow(
            key = Key(Text(messages(s"choose-file-type.file-type.${ds.head.documentType}"))),
            value = Value(
              HtmlContent(
                ds
                  .map(uploadDocument =>
                    Paragraph(
                      uploadDocument.fileName
                    ).toString
                  )
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
}
