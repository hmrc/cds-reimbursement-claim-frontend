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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.UploadFilesMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.upload_files_description

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature

import SecuritiesJourney.Checks._

@Singleton
class UploadFilesController @Inject() (
  val jcc: JourneyControllerComponents,
  uploadDocumentsConnector: UploadDocumentsConnector,
  val uploadDocumentsConfig: UploadDocumentsConfig,
  val fileUploadConfig: FileUploadConfig,
  val upload_files_description: upload_files_description,
  featureSwitchService: FeatureSwitchService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController
    with UploadFilesMixin[SecuritiesJourney] {

  final val precedingAction: Call              = routes.CheckClaimDetailsController.show()
  final val selectDocumentTypePageAction: Call = routes.ChooseFileTypeController.show()
  final val callbackAction: Call               = routes.UploadFilesController.submit()

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getSelectedDocumentTypeOrDefault match {
      case None =>
        logger.warn("missing document type")
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
                  continueAfterNoAnswerUrl,
                  showYesNoQuestionBeforeContinue = journey.needsDocumentTypeSelection,
                  backlinkUrl = Some(
                    if (journey.needsDocumentTypeSelection)
                      selectDocumentTypePageAction.url
                    else
                      precedingAction.url
                  )
                ),
                journey.answers.supportingEvidences
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
          journey
            .receiveUploadedFiles(
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
              Redirect(s"${uploadDocumentsConfig.publicUrl}${uploadDocumentsConfig.contextPath}/summary")

          }
    }
  }

}
