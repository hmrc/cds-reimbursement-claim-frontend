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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator.Validate
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.upload_bod4_description

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class UploadBillOfDischarge4Controller @Inject() (
  val jcc: JourneyControllerComponents,
  uploadDocumentsConnector: UploadDocumentsConnector,
  val uploadDocumentsConfig: UploadDocumentsConfig,
  val fileUploadConfig: FileUploadConfig,
  val upload_bod4_description: upload_bod4_description
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS
        & declarantOrImporterEoriMatchesUserOrHasBeenVerified
        & reasonForSecurityIsENU
    )

  final val backlinkUrl: Call    = routes.CheckDeclarationDetailsController.show
  final val callbackAction: Call = routes.UploadBillOfDischarge4Controller.submit
  final val selfUrl: String      = jcc.servicesConfig.getString("self.url")

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val continueUrl: Call =
      if journey.hasCompleteAnswers then checkYourAnswers
      else routes.AddOtherDocumentsController.show

    uploadDocumentsConnector
      .initialize(
        UploadDocumentsConnector
          .Request(
            uploadDocumentsSessionConfig(
              journey.answers.nonce,
              continueUrl
            ),
            journey.answers.billOfDischargeDocuments
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

  final val submit: Action[AnyContent] = simpleActionReadWriteJourney(
    implicit request =>
      journey =>
        request
          .asInstanceOf[Request[AnyContent]]
          .body
          .asJson
          .flatMap(_.asOpt[UploadDocumentsCallback]) match {
          case None =>
            logger.warn("missing or invalid callback payload")
            (journey, BadRequest("missing or invalid callback payload"))

          case Some(callback) =>
            journey
              .receiveBillOfDischargeDocuments(
                callback.nonce,
                callback.uploadedFiles.map(_.copy(cargo = callback.cargo))
              )
              .fold(
                error => (journey, BadRequest(error)),
                modifiedJourney => (modifiedJourney, NoContent)
              )
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
      maximumNumberOfFiles = fileUploadConfig.readMaxUploadsValue("bill-of-discharge"),
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = fileUploadConfig.readMaxFileSize("bill-of-discharge"),
      allowedContentTypes =
        "application/pdf,image/jpeg,image/png,text/csv,text/plain,application/vnd.ms-outlook,application/msword,application/vnd.openxmlformats-officedocument.wordprocessingml.document,application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/vnd.oasis.opendocument.text,application/vnd.oasis.opendocument.spreadsheet",
      allowedFileExtensions = ".pdf,.png,.jpg,.jpeg,.csv,.txt,.msg,.pst,.ost,.eml,.doc,.docx,.xls,.xlsx,.ods,.odt",
      cargo = Some(UploadDocumentType.BillOfDischarge4),
      newFileDescription = None,
      content = uploadDocumentsContent(),
      features = UploadDocumentsSessionConfig.Features(
        showUploadMultiple = true,
        enableMultipleFilesPicker = true,
        showLanguageSelection = viewConfig.enableLanguageSwitching,
        showAddAnotherDocumentButton = false,
        showYesNoQuestionBeforeContinue = false
      )
    )

  def uploadDocumentsContent(implicit messages: Messages): UploadDocumentsSessionConfig.Content = {
    val descriptionHtml = upload_bod4_description(
      "upload-bill-of-discharge-4"
    )(messages).body
    UploadDocumentsSessionConfig.Content(
      serviceName = messages("service.title"),
      title = messages("upload-bill-of-discharge-4.title"),
      descriptionHtml = descriptionHtml,
      serviceUrl = viewConfig.homePageUrl,
      accessibilityStatementUrl = viewConfig.accessibilityStatementUrl,
      phaseBanner = "beta",
      phaseBannerUrl = viewConfig.betaFeedbackUrl,
      signOutUrl = viewConfig.ggSignOut,
      timedOutUrl = viewConfig.ggTimedOutUrl,
      keepAliveUrl = viewConfig.ggKeepAliveUrl,
      timeoutSeconds = viewConfig.ggTimeoutSeconds,
      countdownSeconds = viewConfig.ggCountdownSeconds,
      pageTitleClasses = "govuk-heading-xl",
      allowedFilesTypesHint = messages("upload-bill-of-discharge-4.allowed-file-types"),
      fileUploadedProgressBarLabel = messages("choose-files.uploaded.label"),
      chooseFirstFileLabel = messages("upload-bill-of-discharge-4.choose.description"),
      fileUploadRequiredError = Some(messages("upload-bill-of-discharge-4.error.file-upload.required"))
    )
  }
}
