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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.{Inject, Singleton}
import play.api.Logging
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, FileUploadConfig, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.ScheduleOfMrnDocumentController.configKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => uploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadDocument, UploadDocumentType, UploadReference, UpscanUpload}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{schedule => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ScheduleOfMrnDocumentController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  upscanService: UpscanService,
  errorHandler: ErrorHandler,
  sessionStore: SessionCache,
  config: FileUploadConfig,
  uploadPage: pages.upload,
  scanProgressPage: pages.scan_progress,
  fileSizeErrorPage: pages.size_fail,
  formatVirusErrorPage: pages.format_virus_fail,
  reviewPage: pages.review
)(implicit viewConfig: ViewConfig, executionContext: ExecutionContext, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  lazy val maxUploads: Int = config.readMaxUploadsValue(configKey)

  implicit val scheduledDocumentExtractor: DraftC285Claim => Option[ScheduledDocumentAnswer] =
    _.scheduledDocumentAnswer

  def uploadScheduledDocument(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ScheduledDocumentAnswer] { (_, answer) =>
        if (answer.toList.length >= maxUploads)
          Future.successful(Redirect(routes.ScheduleOfMrnDocumentController.review()))
        else
          upscanService
            .initiate(
              routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback(),
              routes.ScheduleOfMrnDocumentController.scanProgress,
              config.readMaxFileSize(configKey)
            )
            .fold(_ => errorHandler.errorResult(), upscanUpload => Ok(uploadPage(upscanUpload)))
      }
    }

  def uploadScheduledDocumentSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async {
      Redirect(routes.ScheduleOfMrnDocumentController.uploadScheduledDocument())
    }

  def handleFileSizeErrorCallback(): Action[AnyContent] =
    authenticatedActionWithSessionData {
      Redirect(uploadRoutes.ScheduleOfMrnDocumentController.showFileSizeErrorPage())
    }

  def showFileSizeErrorPage(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(fileSizeErrorPage())
    }

  def addDocument(
    upload: UpscanUpload,
    callback: UpscanSuccess,
    claim: FillingOutClaim
  ): FillingOutClaim = {
    val answer = ScheduledDocumentAnswer(
      UploadDocument(
        upload.uploadReference,
        upload.upscanUploadMeta,
        upload.uploadedOn,
        callback,
        callback.fileName,
        UploadDocumentType.ScheduleOfMRNs.some
      )
    )
    FillingOutClaim.of(claim)(_.copy(scheduledDocumentAnswer = answer.some))
  }

  def scanProgress(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ScheduledDocumentAnswer] { (fillingOut, _) =>
        val result = for {
          upscanUpload <- upscanService getUpscanUpload uploadReference
          _            <- upscanUpload.upscanCallBack match {
                            case Some(upscanSuccess: UpscanSuccess) =>
                              EitherT(
                                updateSession(sessionStore, request)(
                                  _.copy(journeyStatus = addDocument(upscanUpload, upscanSuccess, fillingOut).some)
                                )
                              )
                            case _                                  =>
                              EitherT.pure[Future, Error](())
                          }
        } yield upscanUpload

        result.fold(
          _ => errorHandler.errorResult(),
          upscanUpload =>
            upscanUpload.upscanCallBack match {
              case Some(_: UpscanSuccess) =>
                Redirect(uploadRoutes.ScheduleOfMrnDocumentController.review())
              case Some(_: UpscanFailure) =>
                Redirect(uploadRoutes.ScheduleOfMrnDocumentController.handleFormatOrVirusCheckErrorCallback())
              case None                   =>
                Ok(scanProgressPage(upscanUpload))
            }
        )
      }
    }

  def scanProgressSubmit(
    uploadReference: String
  ): Action[AnyContent] =
    authenticatedActionWithSessionData { _ =>
      Redirect(
        uploadRoutes.ScheduleOfMrnDocumentController.scanProgress(UploadReference(uploadReference))
      )
    }

  def deleteScheduledDocument(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ScheduledDocumentAnswer] { (fillingOutClaim, _) =>
        val newJourney = FillingOutClaim.of(fillingOutClaim)(_.copy(scheduledDocumentAnswer = None))

        val result = for {
          _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney))))
        } yield ()

        result.fold(
          _ => {
            logger.warn(s"Could not update session")
            errorHandler.errorResult()
          },
          _ => Redirect(uploadRoutes.ScheduleOfMrnDocumentController.uploadScheduledDocument())
        )
      }
    }

  def review(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ScheduledDocumentAnswer] { (_, answer) =>
        def redirectToUploadEvidence =
          Redirect(uploadRoutes.ScheduleOfMrnDocumentController.uploadScheduledDocument())

        def listUploadedItems(scheduleDocument: ScheduledDocumentAnswer) =
          Ok(reviewPage(scheduleDocument))

        answer.fold(redirectToUploadEvidence)(listUploadedItems)
      }
    }

  def reviewSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData {
      Redirect(claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Scheduled))
    }

  def handleFormatOrVirusCheckErrorCallback(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(formatVirusErrorPage())
    }
}

object ScheduleOfMrnDocumentController {
  val configKey = "schedule-of-mrn"
}
