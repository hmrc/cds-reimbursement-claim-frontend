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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.upload_files_description

import java.util.Locale

trait UploadFilesMixin {
  self: JourneyBaseController =>

  val uploadDocumentsConfig: UploadDocumentsConfig
  val fileUploadConfig: FileUploadConfig
  val upload_files_description: upload_files_description
  val selectDocumentTypePageAction: Call
  val callbackAction: Call

  final val selfUrl: String = jcc.servicesConfig.getString("self.url")

  def uploadDocumentsSessionConfig(
    nonce: Nonce,
    documentType: UploadDocumentType,
    continueAfterYesAnswerUrl: String,
    continueAfterNoAnswerUrl: String,
    minimumNumberOfFiles: Int = 0, // user can skip uploading the files,
    showYesNoQuestionBeforeContinue: Boolean = true,
    backlinkUrl: Option[String] = None
  )(implicit messages: Messages): UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      nonce = nonce,
      continueUrl = continueAfterNoAnswerUrl,
      continueAfterYesAnswerUrl = Some(continueAfterYesAnswerUrl),
      continueWhenFullUrl = selfUrl + checkYourAnswers.url,
      backlinkUrl = selfUrl + backlinkUrl.getOrElse(selectDocumentTypePageAction.url),
      callbackUrl = uploadDocumentsConfig.callbackUrlPrefix + callbackAction.url,
      minimumNumberOfFiles = minimumNumberOfFiles,
      maximumNumberOfFiles = fileUploadConfig.readMaxUploadsValue("supporting-evidence"),
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = fileUploadConfig.readMaxFileSize("supporting-evidence"),
      allowedContentTypes = "application/pdf,image/jpeg,image/png",
      allowedFileExtensions = "*.pdf,*.png,*.jpg,*.jpeg",
      cargo = Some(documentType),
      newFileDescription = Some(documentTypeDescription(documentType)),
      content = uploadDocumentsContent(documentType),
      features = UploadDocumentsSessionConfig.Features(
        showUploadMultiple = true,
        showLanguageSelection = viewConfig.enableLanguageSwitching,
        showAddAnotherDocumentButton = false,
        showYesNoQuestionBeforeContinue = showYesNoQuestionBeforeContinue
      )
    )

  def uploadDocumentsContent(
    dt: UploadDocumentType
  )(implicit messages: Messages): UploadDocumentsSessionConfig.Content = {
    val documentTypeLabel = documentTypeDescription(dt).toLowerCase(Locale.ENGLISH)
    val descriptionHtml   = upload_files_description(
      "choose-files.rejected-goods",
      documentTypeLabel
    )(messages).body

    UploadDocumentsSessionConfig.Content(
      serviceName = messages("service.title"),
      title = messages("choose-files.rejected-goods.title", documentTypeLabel),
      descriptionHtml = descriptionHtml,
      serviceUrl = viewConfig.homePageUrl,
      accessibilityStatementUrl = viewConfig.accessibilityStatementUrl,
      phaseBanner = "beta",
      phaseBannerUrl = viewConfig.serviceFeedBackUrl,
      signOutUrl = viewConfig.signOutUrl,
      timedOutUrl = viewConfig.ggTimedOutUrl,
      keepAliveUrl = viewConfig.ggKeepAliveUrl,
      timeoutSeconds = viewConfig.ggTimeoutSeconds.toInt,
      countdownSeconds = viewConfig.ggCountdownSeconds.toInt,
      pageTitleClasses = "govuk-heading-xl",
      allowedFilesTypesHint = messages("choose-files.rejected-goods.allowed-file-types"),
      fileUploadedProgressBarLabel = messages("choose-files.uploaded.label"),
      chooseFirstFileLabel = messages("choose-files.rejected-goods.choose.first.label", documentTypeLabel),
      chooseNextFileLabel = Some(messages("choose-files.rejected-goods.choose.next.label", documentTypeLabel)),
      addAnotherDocumentButtonText = Some(messages("choose-files.rejected-goods.choose.next.label", documentTypeLabel)),
      yesNoQuestionText = Some(messages("choose-files.rejected-goods.add-another-document-question")),
      yesNoQuestionRequiredError =
        Some(messages("choose-files.rejected-goods.add-another-document-question.error.required"))
    )
  }

  def documentTypeDescription(dt: UploadDocumentType)(implicit messages: Messages): String =
    messages(s"choose-file-type.file-type.${UploadDocumentType.keyOf(dt)}")

}
