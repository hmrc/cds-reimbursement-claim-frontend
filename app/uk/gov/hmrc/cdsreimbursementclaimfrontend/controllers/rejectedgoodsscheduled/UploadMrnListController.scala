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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadMrnListCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.upload_mrn_list_description

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class UploadMrnListController @Inject() (
  val jcc: JourneyControllerComponents,
  uploadDocumentsConnector: UploadDocumentsConnector,
  val uploadDocumentsConfig: UploadDocumentsConfig,
  val fileUploadConfig: FileUploadConfig,
  val upload_mrn_list_description: upload_mrn_list_description,
  featureSwitchService: FeatureSwitchService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  final val backlinkUrl: Call    = routes.CheckDeclarationDetailsController.show()
  final val callbackAction: Call = routes.UploadMrnListController.submit()
  final val selfUrl: String      = jcc.servicesConfig.getString("self.url")

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val continueUrl: Call =
      if (journey.hasCompleteAnswers) checkYourAnswers
      else routes.CheckClaimantDetailsController.show()

    uploadDocumentsConnector
      .initialize(
        UploadDocumentsConnector
          .Request(
            uploadDocumentsSessionConfig(
              journey.answers.nonce,
              continueUrl
            ),
            journey.answers.scheduledDocument.map(file => Seq(file)).getOrElse(Seq.empty),
            featureSwitchService
              .optionally(Feature.InternalUploadDocuments, "schedule-document")
          )
      )
      .map {
        case Some(url) =>
          Redirect(url)
        case None      =>
          Redirect(
            s"${uploadDocumentsConfig.publicUrl}${uploadDocumentsConfig.contextPath}"
          )
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final val submit: Action[AnyContent] = simpleActionReadWriteJourney(
    { implicit request => journey =>
      request
        .asInstanceOf[Request[AnyContent]]
        .body
        .asJson
        .flatMap(_.asOpt[UploadMrnListCallback]) match {
        case None =>
          logger.warn("missing or invalid callback payload")
          (journey, BadRequest("missing or invalid callback payload"))

        case Some(callback) =>
          callback.uploadedFiles.headOption match {
            case Some(uploadedFile) =>
              journey
                .receiveScheduledDocument(
                  callback.nonce,
                  uploadedFile
                )
                .fold(
                  error => (journey, BadRequest(error)),
                  modifiedJourney => (modifiedJourney, NoContent)
                )
            case None               =>
              (journey.removeScheduledDocument, NoContent)
          }

      }
    },
    isCallback = true
  )

  def uploadDocumentsSessionConfig(
    nonce: Nonce,
    continueUrl: Call
  )(implicit messages: Messages): UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      nonce = nonce,
      continueUrl = selfUrl + continueUrl.url,
      continueWhenFullUrl = selfUrl + continueUrl.url,
      callbackUrl = uploadDocumentsConfig.callbackUrlPrefix + callbackAction.url,
      minimumNumberOfFiles = 1,
      maximumNumberOfFiles = 1,
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = fileUploadConfig.readMaxFileSize("schedule-of-mrn"),
      allowedContentTypes = "application/pdf,image/jpeg,image/png",
      allowedFileExtensions = ".pdf,.png,.jpg,.jpeg",
      cargo = Some(UploadDocumentType.ScheduleOfMRNs),
      newFileDescription =
        Some(messages(s"schedule-document.file-type.${UploadDocumentType.keyOf(UploadDocumentType.ScheduleOfMRNs)}")),
      content = uploadDocumentsContent,
      features = UploadDocumentsSessionConfig.Features(
        showUploadMultiple = true,
        showLanguageSelection = viewConfig.enableLanguageSwitching,
        showAddAnotherDocumentButton = false,
        showYesNoQuestionBeforeContinue = false
      )
    )

  def uploadDocumentsContent(implicit messages: Messages): UploadDocumentsSessionConfig.Content = {
    val descriptionHtml = upload_mrn_list_description(
      "schedule-document.upload"
    )(messages).body

    UploadDocumentsSessionConfig.Content(
      serviceName = messages("service.title"),
      title = messages("schedule-document.upload.title"),
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
      allowedFilesTypesHint = messages("schedule-document.upload.allowed-file-types"),
      fileUploadedProgressBarLabel = messages("choose-files.uploaded.label"),
      chooseFirstFileLabel = messages("schedule-document.upload.choose.description")
    )
  }
}
