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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile

import java.util.Locale

trait UploadFilesMixin extends JourneyBaseController {

  val uploadDocumentsConnector: UploadDocumentsConnector
  val uploadDocumentsConfig: UploadDocumentsConfig
  val fileUploadConfig: FileUploadConfig
  val selectDocumentTypePageAction: Call
  val callbackAction: Call

  def chooseFilesPageDescriptionTemplate: String => Messages => HtmlFormat.Appendable

  def modifyJourney(
    journey: Journey,
    documentType: UploadDocumentType,
    requestNonce: Nonce,
    uploadedFiles: Seq[UploadedFile]
  ): Either[String, Journey]

  final val selfUrl: String = jcc.servicesConfig.getString("self.url")

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.selectedDocumentType match {
      case None =>
        Redirect(selectDocumentTypePageAction).asFuture

      case Some(documentType) =>
        val continueAfterYesAnswerUrl =
          selfUrl + selectDocumentTypePageAction.url

        val continueAfterNoAnswerUrl =
          selfUrl + checkYourAnswers.url

        uploadDocumentsConnector
          .initialize(
            UploadDocumentsConnector
              .Request(
                uploadDocumentsSessionConfig(
                  journey.answers.nonce,
                  documentType,
                  continueAfterYesAnswerUrl,
                  continueAfterNoAnswerUrl
                ),
                journey.answers.supportingEvidences
                  .map(file => file.copy(description = file.documentType.map(documentTypeDescription _)))
              )
          )
          .map {
            case Some(url) =>
              Redirect(url)

            case None =>
              Redirect(
                s"${uploadDocumentsConfig.publicUrl}${uploadDocumentsConfig.contextPath}"
              )
          }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final val submit: Action[AnyContent] = simpleActionReadWriteJourney(
    { implicit request => journey =>
      request
        .asInstanceOf[Request[AnyContent]]
        .body
        .asJson
        .flatMap(_.asOpt[UploadDocumentsCallback]) match {
        case None =>
          logger.warn("missing or invalid callback payload")
          (journey, BadRequest("missing or invalid callback payload"))

        case Some(callback) =>
          modifyJourney(
            journey,
            callback.documentType,
            callback.nonce,
            callback.uploadedFiles.map(_.copy(description = None))
          )
            .fold(
              error => (journey, BadRequest(error)),
              modifiedJourney => (modifiedJourney, NoContent)
            )
      }
    },
    isCallback = true
  )

  final val summary: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.selectedDocumentType match {
      case None =>
        Redirect(selectDocumentTypePageAction).asFuture

      case Some(documentType) =>
        val continueAfterYesAnswerUrl =
          selfUrl + selectDocumentTypePageAction.url

        val continueAfterNoAnswerUrl =
          selfUrl + checkYourAnswers.url

        uploadDocumentsConnector
          .initialize(
            UploadDocumentsConnector
              .Request(
                uploadDocumentsSessionConfig(
                  journey.answers.nonce,
                  documentType,
                  continueAfterYesAnswerUrl,
                  continueAfterNoAnswerUrl
                ),
                journey.answers.supportingEvidences
                  .map(file => file.copy(description = file.documentType.map(documentTypeDescription _)))
              )
          )
          .map {
            case Some(url) =>
              Redirect(url)
            case None      =>
              Redirect(s"${uploadDocumentsConfig.publicUrl}${uploadDocumentsConfig.contextPath}/summary")
          }
    }
  }

  def uploadDocumentsSessionConfig(
    nonce: Nonce,
    documentType: UploadDocumentType,
    continueAfterYesAnswerUrl: String,
    continueAfterNoAnswerUrl: String,
    minimumNumberOfFiles: Int = 0, // user can skip uploading the files,
    showYesNoQuestionBeforeContinue: Boolean = true
  )(implicit messages: Messages): UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      nonce = nonce,
      continueUrl = continueAfterNoAnswerUrl,
      continueAfterYesAnswerUrl = Some(continueAfterYesAnswerUrl),
      continueWhenFullUrl = selfUrl + checkYourAnswers.url,
      callbackUrl = uploadDocumentsConfig.callbackUrlPrefix + callbackAction.url,
      minimumNumberOfFiles = minimumNumberOfFiles,
      maximumNumberOfFiles = fileUploadConfig.readMaxUploadsValue("supporting-evidence"),
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = fileUploadConfig.readMaxFileSize("supporting-evidence"),
      allowedContentTypes = "application/pdf,image/jpeg,image/png",
      allowedFileExtensions = ".pdf,.png,.jpg,.jpeg",
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
    val descriptionHtml   = chooseFilesPageDescriptionTemplate(
      documentTypeLabel
    )(messages).body

    UploadDocumentsSessionConfig.Content(
      serviceName = messages("service.title"),
      title = messages("choose-files.rejected-goods.title", documentTypeLabel),
      descriptionHtml = descriptionHtml,
      serviceUrl = viewConfig.homePageUrl,
      accessibilityStatementUrl = viewConfig.accessibilityStatementUrl,
      phaseBanner = "beta",
      phaseBannerUrl = viewConfig.betaFeedbackUrl,
      signOutUrl = viewConfig.ggSignOut,
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
