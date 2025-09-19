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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.govukfrontend.views.Aliases.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils.SeqOps

object CheckYourAnswersSupportingDocumentsCardSummary {

  def apply(
    documents: Seq[EvidenceDocument],
    changeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    val isPrintView                                                 = changeCallOpt.isEmpty
    val commonDocs                                                  = documents.filterNot(a => UploadDocumentType.isUploadedSeparately(a.documentType))
    val documentMap: Map[UploadDocumentType, Seq[EvidenceDocument]] = commonDocs.groupBy(_.documentType)
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages("check-your-answers.attached-documents.uploaded-files"))),
          value = Value(
            if commonDocs.isEmpty then Text(messages("check-your-answers.attached-documents.empty"))
            else
              HtmlContent(
                documentMap
                  .flatMap { case (docType, docs) =>
                    Seq(
                      s"<p class='govuk-body govuk-!-margin-bottom-0'>${messages(s"choose-file-type.file-type.$docType")}:</p>"
                    ) ++
                      "<ul class='govuk-list'>" ++
                      docs.map { doc =>
                        if (isPrintView) {
                          s"<li>${doc.fileName}</li>"
                        } else {
                          doc.previewUrl
                            .map(previewUrl =>
                              s"<li><a href='$previewUrl' target='_blank' class='govuk-link'>${doc.fileName}</a></li>"
                            )
                            .getOrElse(s"<li>${doc.fileName}</li>")
                        }
                      } ++ "</ul>"
                  }
                  .mkString("")
              )
          ),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages("check-your-answers.attached-documents.uploaded-files.hidden"))
                )
              )
            )
          )
        )
      )
    )

  def withSeparatelyUploadedDocuments(
    documents: Seq[EvidenceDocument],
    changeCallOpt: Option[Call],
    separateChangeCallOpt: Option[PartialFunction[UploadDocumentType, Call]],
    addOtherDocumentsChangeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList =
    val isPrintView                                                   = changeCallOpt.isEmpty
    val commonDocs: Seq[EvidenceDocument]                             =
      documents.filterNot(a => UploadDocumentType.isUploadedSeparately(a.documentType))
    val commonDocsMap: Map[UploadDocumentType, Seq[EvidenceDocument]] =
      commonDocs.groupBy(_.documentType)

    val billOfDischargeDocuments: Option[Seq[EvidenceDocument]] =
      documents
        .filter(a =>
          a.documentType == UploadDocumentType.BillOfDischarge3
            || a.documentType == UploadDocumentType.BillOfDischarge4
        )
        .noneIfEmpty

    val proofOfOriginDocuments: Option[Seq[EvidenceDocument]] =
      documents.filter(a => a.documentType == UploadDocumentType.ProofOfOrigin).noneIfEmpty

    SummaryList.optionally(
      billOfDischargeDocuments.map(documents => singleDocumentTypeRow(documents, isPrintView, separateChangeCallOpt)),
      proofOfOriginDocuments.map(documents => singleDocumentTypeRow(documents, isPrintView, separateChangeCallOpt)),
      Some(
        SummaryListRow(
          key = Key(Text(messages("check-your-answers.attached-documents.add-other-documents.label"))),
          value = Value(
            Text(
              messages(
                if commonDocs.isEmpty
                then s"check-your-answers.claim-for-security.claim-full-amount.no"
                else s"check-your-answers.claim-for-security.claim-full-amount.yes"
              )
            )
          ),
          actions = addOtherDocumentsChangeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText =
                    Some(messages("check-your-answers.attached-documents.add-other-documents.hidden"))
                )
              )
            )
          )
        )
      ),
      Some(
        SummaryListRow(
          key = Key(Text(messages("check-your-answers.attached-documents.other-uploaded-files"))),
          value = Value(
            if commonDocs.isEmpty then Text(messages("check-your-answers.attached-documents.empty"))
            else
              HtmlContent(
                commonDocsMap
                  .flatMap { case (docType, docs) =>
                    Seq(
                      s"<p class='govuk-body govuk-!-margin-bottom-0'>${messages(s"choose-file-type.file-type.$docType")}:</p>"
                    ) ++
                      "<ul class='govuk-list'>" ++
                      docs.map { doc =>
                        if (isPrintView) {
                          s"<li>${doc.fileName}</li>"
                        } else {
                          doc.previewUrl
                            .map(previewUrl =>
                              s"<li><a href='$previewUrl' target='_blank' class='govuk-link'>${doc.fileName}</a></li>"
                            )
                            .getOrElse(s"<li>${doc.fileName}</li>")
                        }
                      } ++ "</ul>"
                  }
                  .mkString("")
              )
          ),
          actions = changeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText =
                    Some(messages("check-your-answers.attached-documents.other-uploaded-files.hidden"))
                )
              )
            )
          )
        )
      )
    )

  def singleDocumentTypeRow(
    documents: Seq[EvidenceDocument],
    isPrintView: Boolean,
    changeCallOpt: Option[PartialFunction[UploadDocumentType, Call]]
  )(implicit
    messages: Messages
  ) = {
    val documentType = documents.head.documentType
    SummaryListRow(
      key = Key(
        Text(
          messages(
            s"check-your-answers.attached-documents.${UploadDocumentType.keyOf(documentType)}.label"
          )
        )
      ),
      value = Value(
        if documents.isEmpty
        then Text(messages("check-your-answers.attached-documents.empty"))
        else
          HtmlContent(
            ("<ul class='govuk-list'>" ++
              documents.map { doc =>
                if (isPrintView) {
                  s"<li>${doc.fileName}</li>"
                } else {
                  doc.previewUrl
                    .map(previewUrl =>
                      s"<li><a href='$previewUrl' target='_blank' class='govuk-link'>${doc.fileName}</a></li>"
                    )
                    .getOrElse(s"<li>${doc.fileName}</li>")
                }
              } ++ "</ul>").mkString
          )
      ),
      actions = changeCallOpt.map(changeCall =>
        Actions(
          items = Seq(
            ActionItem(
              href = changeCall(documentType).url,
              content = Text(messages("cya.change")),
              visuallyHiddenText = Some(
                messages(s"check-your-answers.attached-documents.${UploadDocumentType.keyOf(documentType)}.hidden")
              )
            )
          )
        )
      )
    )
  }

}
