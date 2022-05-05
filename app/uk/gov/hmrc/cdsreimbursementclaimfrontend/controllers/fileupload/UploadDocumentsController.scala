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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload

import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsCallbackSender
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ModelBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionModel
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.fileupload.choose_file
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.fileupload.scan_failed
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.fileupload.scan_progress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.fileupload.summary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.fileupload.upload_failed

import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.AsInstanceOf"))
@Singleton
class UploadDocumentsController @Inject() (
  val ccc: JourneyControllerComponents,
  upscanService: UpscanService,
  uploadDocumentsCallbackSender: UploadDocumentsCallbackSender,
  chooseFilePage: choose_file,
  summaryPage: summary,
  scanProgressPage: scan_progress,
  uploadFailedPage: upload_failed,
  scanFailedPage: scan_failed
)(implicit
  executionContext: ExecutionContext,
  viewConfig: ViewConfig
) extends ModelBaseController[UploadDocumentsSessionModel] {

  override val requiredFeature: Option[Feature] = None

  override def getModel(sessionData: SessionData): Option[UploadDocumentsSessionModel] =
    sessionData.uploadDocumentsSessionModel

  override def updateModel(sessionData: SessionData, model: UploadDocumentsSessionModel): SessionData =
    sessionData.copy(uploadDocumentsSessionModel = Some(model))

  override val fallbackResultIfModelMissing: Result =
    Redirect(viewConfig.homePageUrl)

  private val unmodified: UploadDocumentsSessionModel => UploadDocumentsSessionModel =
    identity[UploadDocumentsSessionModel] _

  val show: Action[AnyContent] =
    actionReadModel { implicit request => model =>
      if (model.uploadedFiles.size >= model.sessionConfig.maximumNumberOfFiles)
        Redirect(routes.UploadDocumentsController.summary()).asFuture
      else
        upscanService
          .initiate(
            routes.UploadDocumentsController.handleUpscanErrorRedirect(),
            reference => routes.UploadDocumentsController.scanProgress(reference.value),
            model.sessionConfig.maximumFileSizeBytes
          )
          .fold(
            _ => ccc.errorHandler.errorResult(),
            upscanUpload => Ok(chooseFilePage(upscanUpload, model.sessionConfig, s"${model.internalKey}.upload"))
          )
    }

  val redirectToShow: Action[AnyContent] =
    Action(_ => Redirect(routes.UploadDocumentsController.show()))

  def scanProgress(reference: String): Action[AnyContent] =
    actionReadUpdateModel { implicit request => model =>
      upscanService
        .getUpscanUpload(UploadReference(reference))
        .value
        .flatMap {
          case Left(_)             => (unmodified, ccc.errorHandler.errorResult()).asFuture
          case Right(upscanUpload) =>
            upscanUpload.upscanCallBack match {
              case Some(upscanSuccess: UpscanSuccess)
                  if model.uploadedFiles.forall(_.upscanReference =!= reference) &&
                    model.uploadedFiles.forall(_.checksum =!= upscanSuccess.uploadDetails.checksum) =>
                val updatedFiles =
                  model.uploadedFiles :+ UploadedFile
                    .from(UploadReference(reference), upscanSuccess)
                    .copy(cargo = model.sessionConfig.cargo)

                uploadDocumentsCallbackSender
                  .sendUploadedFiles(
                    model.sessionConfig.callbackUrl,
                    model.sessionConfig.nonce,
                    updatedFiles,
                    model.sessionConfig.cargo
                  )
                  .map(_ =>
                    (
                      (m: UploadDocumentsSessionModel) => m.copy(uploadedFiles = updatedFiles),
                      Redirect(routes.UploadDocumentsController.summary())
                    )
                  )

              case Some(_: UpscanSuccess) =>
                (unmodified, Redirect(routes.UploadDocumentsController.summary())).asFuture

              case Some(upscanFailure: UpscanFailure) =>
                logger.info(s"Received upscan failure callback: $upscanFailure")
                (unmodified, Redirect(routes.UploadDocumentsController.showScanErrorPage())).asFuture

              case None =>
                logger.info(s"No upscan callback registered yet for $reference")
                (
                  unmodified,
                  Ok(
                    scanProgressPage(
                      routes.UploadDocumentsController.scanProgressSubmit(reference),
                      s"${model.internalKey}.scan-progress"
                    )
                  )
                ).asFuture
            }
        }
    }

  def scanProgressSubmit(reference: String): Action[AnyContent] = Action { _ =>
    Redirect(routes.UploadDocumentsController.scanProgress(reference))
  }

  def remove(reference: String): Action[AnyContent] =
    actionReadUpdateModel { implicit request => model =>
      val updatedFiles = model.uploadedFiles
        .filterNot(_.upscanReference === reference)

      uploadDocumentsCallbackSender
        .sendUploadedFiles(
          model.sessionConfig.callbackUrl,
          model.sessionConfig.nonce,
          updatedFiles,
          model.sessionConfig.cargo
        )
        .map(_ =>
          (
            (m: UploadDocumentsSessionModel) => m.copy(uploadedFiles = updatedFiles),
            Redirect(
              if (updatedFiles.isEmpty)
                model.sessionConfig.continueAfterYesAnswerUrl
                  .getOrElse(routes.UploadDocumentsController.show().url)
              else
                routes.UploadDocumentsController.summary().url
            )
          )
        )
    }

  val handleUpscanErrorRedirect: Action[AnyContent] =
    Action(_ => Redirect(routes.UploadDocumentsController.showUploadErrorPage()))

  val showUploadErrorPage: Action[AnyContent] =
    actionReadModel { implicit request => model =>
      Ok(
        uploadFailedPage(
          routes.UploadDocumentsController.redirectToShow(),
          s"${model.internalKey}.upload-failed"
        )
      )
    }

  val showScanErrorPage: Action[AnyContent] =
    actionReadModel { implicit request => model =>
      Ok(
        scanFailedPage(
          routes.UploadDocumentsController.redirectToShow(),
          s"${model.internalKey}.scan-failed"
        )
      )
    }

  val summary: Action[AnyContent] =
    actionReadModel { implicit request => model =>
      if (model.uploadedFiles.isEmpty)
        Redirect(
          model.sessionConfig.continueAfterYesAnswerUrl
            .getOrElse(routes.UploadDocumentsController.show().url)
        )
      else
        Ok(
          summaryPage(
            model.uploadedFiles,
            model.canShowYesNoQuestion,
            YesOrNoQuestionForm(s"${model.internalKey}.check-your-answers"),
            s"${model.internalKey}.check-your-answers",
            routes.UploadDocumentsController.summarySubmit(),
            (reference: String) => routes.UploadDocumentsController.remove(reference)
          )
        )
    }

  val summarySubmit: Action[AnyContent] =
    actionReadModel { implicit request => model =>
      YesOrNoQuestionForm(s"${model.internalKey}.check-your-answers")
        .bindFromRequest()
        .fold(
          formWithErrors =>
            BadRequest(
              summaryPage(
                model.uploadedFiles,
                model.canShowYesNoQuestion,
                formWithErrors,
                s"${model.internalKey}.check-your-answers",
                routes.UploadDocumentsController.summarySubmit(),
                (reference: String) => routes.UploadDocumentsController.remove(reference)
              )
            ),
          yesNo =>
            Redirect(yesNo match {
              case Yes =>
                if (model.uploadedFiles.size === model.sessionConfig.maximumNumberOfFiles)
                  model.sessionConfig.continueAfterYesAnswerUrl
                    .getOrElse(model.sessionConfig.continueWhenFullUrl)
                else
                  model.sessionConfig.continueAfterYesAnswerUrl
                    .getOrElse(model.sessionConfig.continueUrl)

              case No =>
                if (model.uploadedFiles.size === model.sessionConfig.maximumNumberOfFiles)
                  model.sessionConfig.continueWhenFullUrl
                else
                  model.sessionConfig.continueUrl
            })
        )
    }

}
