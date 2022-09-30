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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.supporting_evidence_description
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext

@Singleton
class UploadFilesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  uploadDocumentsConnector: UploadDocumentsConnector,
  uploadDocumentsConfig: UploadDocumentsConfig,
  fileUploadConfig: FileUploadConfig,
  featureSwitchService: FeatureSwitchService,
  servicesConfig: ServicesConfig,
  supporting_evidence_description: supporting_evidence_description
)(implicit
  viewConfig: ViewConfig,
  ec: ExecutionContext,
  val controllerComponents: MessagesControllerComponents,
  errorHandler: ErrorHandler
) extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  final val pageKey: String = "supporting-evidence"

  final val selfUrl: String = servicesConfig.getString("self.url")

  final val evidenceTypes: Seq[UploadDocumentType] = UploadDocumentType.c285DocumentTypes

  final val show: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        fillingOutClaim.draftClaim.documentTypeAnswer match {
          case None =>
            Redirect(routes.ChooseFileTypeController.show)

          case Some(documentType) =>
            val continueAfterYesAnswerUrl =
              selfUrl + routes.ChooseFileTypeController.show.url

            val continueAfterNoAnswerUrl =
              selfUrl + routes.CheckYourAnswersAndSubmitController.checkAllAnswers.url

            uploadDocumentsConnector
              .initialize(
                UploadDocumentsConnector
                  .Request(
                    uploadDocumentsSessionConfig(
                      fillingOutClaim.draftClaim.nonce,
                      documentType,
                      continueAfterYesAnswerUrl,
                      continueAfterNoAnswerUrl
                    ),
                    fillingOutClaim.draftClaim.supportingEvidencesAnswer
                      .map(_.toList)
                      .getOrElse(List.empty)
                      .map(file => file.copy(description = file.documentType.map(documentTypeDescription _))),
                    featureSwitchService
                      .optionally(Feature.InternalUploadDocuments, "supporting-evidence")
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
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  final val callback: Action[AnyContent] =
    authenticatedCallbackWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        request.body.asJson
          .flatMap(_.asOpt[UploadDocumentsCallback]) match {
          case None =>
            BadRequest(s"missing or invalid callback payload")

          case Some(callback) =>
            if (callback.nonce.equals(fillingOutClaim.draftClaim.nonce)) {
              val uploadedFilesWithDocumentTypeAdded = callback.uploadedFiles.map {
                case uf if uf.documentType.isEmpty => uf.copy(cargo = Some(callback.documentType))
                case uf                            => uf
              }
              EitherT(
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus = FillingOutClaim
                      .from(fillingOutClaim)(
                        _.copy(supportingEvidencesAnswer = uploadedFilesWithDocumentTypeAdded.someNelOrNone)
                      )
                      .some
                  )
                )
              )
                .fold(
                  logAndDisplayError("Error assigning evidence document type"),
                  _ => NoContent
                )
            } else
              BadRequest("invalid nonce")
        }

      }
    }

  final val summary: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        fillingOutClaim.draftClaim.documentTypeAnswer match {
          case None =>
            Redirect(routes.ChooseFileTypeController.show)

          case Some(documentType) =>
            val continueAfterYesAnswerUrl =
              selfUrl + routes.ChooseFileTypeController.show.url

            val continueAfterNoAnswerUrl =
              selfUrl + routes.CheckYourAnswersAndSubmitController.checkAllAnswers.url

            uploadDocumentsConnector
              .initialize(
                UploadDocumentsConnector
                  .Request(
                    uploadDocumentsSessionConfig(
                      fillingOutClaim.draftClaim.nonce,
                      documentType,
                      continueAfterYesAnswerUrl,
                      continueAfterNoAnswerUrl
                    ),
                    fillingOutClaim.draftClaim.supportingEvidencesAnswer
                      .map(_.toList)
                      .getOrElse(List.empty)
                      .map(file => file.copy(description = file.documentType.map(documentTypeDescription _))),
                    featureSwitchService
                      .optionally(Feature.InternalUploadDocuments, "supporting-evidence")
                  )
              )
              .map {
                case Some(url) =>
                  Redirect(
                    if (featureSwitchService.isEnabled(Feature.InternalUploadDocuments))
                      uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.routes.UploadDocumentsController
                        .summary()
                        .url
                    else
                      url
                  )

                case None =>
                  Redirect(
                    s"${uploadDocumentsConfig.publicUrl}${uploadDocumentsConfig.contextPath}"
                  )
              }

        }
      }
    }

  def getSupportingEvidenceHints(documentTypeList: Seq[UploadDocumentType]): DropdownHints =
    DropdownHints.enumeration(documentTypeList)

  /** Configuration of the upload files session in the external mini-journey */
  def uploadDocumentsSessionConfig(
    nonce: Nonce,
    documentType: UploadDocumentType,
    continueAfterYesAnswerUrl: String,
    continueAfterNoAnswerUrl: String
  )(implicit messages: Messages): UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      nonce = nonce,
      continueUrl = continueAfterNoAnswerUrl,
      continueAfterYesAnswerUrl = Some(continueAfterYesAnswerUrl),
      continueWhenFullUrl = selfUrl + routes.CheckYourAnswersAndSubmitController.checkAllAnswers.url,
      backlinkUrl = selfUrl + routes.ChooseFileTypeController.show.url,
      callbackUrl = uploadDocumentsConfig.callbackUrlPrefix + routes.UploadFilesController.callback.url,
      minimumNumberOfFiles = 0, // user can skip uploading the files
      maximumNumberOfFiles = fileUploadConfig.readMaxUploadsValue("supporting-evidence"),
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = fileUploadConfig.readMaxFileSize("supporting-evidence"),
      allowedContentTypes = "application/pdf,image/jpeg,image/png",
      allowedFileExtensions = "*.pdf,*.png,*.jpg,*.jpeg",
      cargo = Some(documentType),
      newFileDescription = Some(documentTypeDescription(documentType)),
      content = uploadDocumentsContent,
      features = UploadDocumentsSessionConfig.Features(
        showUploadMultiple = true,
        showLanguageSelection = viewConfig.enableLanguageSwitching,
        showAddAnotherDocumentButton = false,
        showYesNoQuestionBeforeContinue = true
      )
    )

  /** External upload files mini-journey content customization. */
  def uploadDocumentsContent(implicit messages: Messages): UploadDocumentsSessionConfig.Content = {
    val descriptionHtml = supporting_evidence_description(
      pageKey,
      getSupportingEvidenceHints(evidenceTypes)
    )(messages).body

    UploadDocumentsSessionConfig.Content(
      serviceName = messages("service.title"),
      title = messages(s"$pageKey.upload.title"),
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
      allowedFilesTypesHint = messages(s"$pageKey.upload.allowed-file-types"),
      fileUploadedProgressBarLabel = messages("choose-files.uploaded.label"),
      chooseFirstFileLabel = messages(s"$pageKey.upload.input-title"),
      chooseNextFileLabel = None,
      addAnotherDocumentButtonText = Some(messages(s"$pageKey.upload.input-title")),
      yesNoQuestionText = Some(messages(s"$pageKey.check-your-answers.add-another-document-question")),
      yesNoQuestionRequiredError = Some(messages(s"$pageKey.check-your-answers.error.required"))
    )
  }

  def documentTypeDescription(dt: UploadDocumentType)(implicit messages: Messages): String =
    messages(s"$pageKey.choose-document-type.document-type.${UploadDocumentType.keyOf(dt)}")

}
