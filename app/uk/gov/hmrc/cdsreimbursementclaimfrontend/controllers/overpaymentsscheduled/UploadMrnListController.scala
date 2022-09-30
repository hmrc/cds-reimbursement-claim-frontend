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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadMrnListCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.upload_mrn_list_description
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext

@Singleton
class UploadMrnListController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  uploadDocumentsConnector: UploadDocumentsConnector,
  uploadDocumentsConfig: UploadDocumentsConfig,
  fileUploadConfig: FileUploadConfig,
  featureSwitchService: FeatureSwitchService,
  servicesConfig: ServicesConfig,
  upload_mrn_list_description: upload_mrn_list_description
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

  final val pageKey: String = "schedule-document"

  final val selfUrl: String     = servicesConfig.getString("self.url")
  final val continueUrl: String = selfUrl + routes.UploadMrnListController.continue.url

  final val show: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        uploadDocumentsConnector
          .initialize(
            UploadDocumentsConnector
              .Request(
                uploadDocumentsSessionConfig(
                  fillingOutClaim.draftClaim.nonce
                ),
                fillingOutClaim.draftClaim.scheduledDocumentAnswer
                  .map(a => Seq(a.uploadDocument))
                  .getOrElse(Seq.empty),
                featureSwitchService
                  .optionally(Feature.InternalUploadDocuments, "schedule-document")
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

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  final val callback: Action[AnyContent] =
    authenticatedCallbackWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        request.body.asJson
          .flatMap(_.asOpt[UploadMrnListCallback]) match {
          case None =>
            BadRequest("missing or invalid callback payload")

          case Some(callback) =>
            if (callback.nonce.equals(fillingOutClaim.draftClaim.nonce))
              EitherT(
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus = FillingOutClaim
                      .from(fillingOutClaim)(
                        _.copy(scheduledDocumentAnswer =
                          callback.uploadedFiles.headOption.map(ScheduledDocumentAnswer.apply)
                        )
                      )
                      .some
                  )
                )
              )
                .fold(
                  logAndDisplayError("Error assigning schedule document"),
                  _ => NoContent
                )
            else {
              BadRequest("invalid nonce")
            }
        }

      }
    }

  final val continue: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case journey: FillingOutClaim =>
        request
          .routeToCheckAnswers(JourneyBindable.Scheduled)
          .whenComplete(journey.draftClaim)(alternatively =
            OverpaymentsRoutes.CheckContactDetailsController.show(JourneyBindable.Scheduled)
          )
      }
    }

  /** Configuration of the upload files session in the external mini-journey */
  def uploadDocumentsSessionConfig(
    nonce: Nonce
  )(implicit messages: Messages): UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      nonce = nonce,
      continueUrl = continueUrl,
      continueWhenFullUrl = continueUrl,
      backlinkUrl = selfUrl + overpaymentsScheduledRoutes.CheckDeclarationDetailsController.show.url,
      callbackUrl = uploadDocumentsConfig.callbackUrlPrefix + routes.UploadMrnListController.callback.url,
      minimumNumberOfFiles = 1,
      maximumNumberOfFiles = 1,
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = fileUploadConfig.readMaxFileSize("schedule-of-mrn"),
      allowedContentTypes = "application/pdf,image/jpeg,image/png",
      allowedFileExtensions = "*.pdf,*.png,*.jpg,*.jpeg",
      cargo = Some(UploadDocumentType.ScheduleOfMRNs),
      newFileDescription =
        Some(messages(s"$pageKey.file-type.${UploadDocumentType.keyOf(UploadDocumentType.ScheduleOfMRNs)}")),
      content = uploadDocumentsContent,
      features = UploadDocumentsSessionConfig.Features(
        showUploadMultiple = true,
        showLanguageSelection = viewConfig.enableLanguageSwitching,
        showAddAnotherDocumentButton = false,
        showYesNoQuestionBeforeContinue = false
      )
    )

  /** External upload files mini-journey content customization. */
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
