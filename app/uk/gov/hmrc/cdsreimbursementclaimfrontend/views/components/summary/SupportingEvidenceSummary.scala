/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController.supportingEvidenceKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.govukfrontend.views.Aliases.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{HtmlContent, Text}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ActionItem, Key, SummaryList, SummaryListRow, Value}

class SupportingEvidenceSummary extends AnswerSummary[SupportingEvidencesAnswer] {

  def render(key: String, answers: SupportingEvidencesAnswer)(implicit
    journey: JourneyBindable,
    messages: Messages
  ): SummaryList = {

    def renderDetails(uploadDocument: UploadDocument): String = {
      val documentName = uploadDocument.fileName
      val documentType = uploadDocument.documentType
        .map(_.index)
        .map { index =>
          messages(s"$supportingEvidenceKey.choose-document-type.document-type.d$index")
        }

      s"$documentName${documentType.fold("")(documentType => s"<br />$documentType")}"
    }

    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.label"))),
          value = Value(
            HtmlContent(
              answers
                .map(answer => s"""<p class="govuk-body">${renderDetails(answer)}</p>""".stripMargin)
                .toList
                .mkString("")
            )
          ),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.SupportingEvidenceController.checkYourAnswers(journey).url}",
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
}
