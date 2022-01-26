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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsCallback
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import play.api.i18n.Messages

@Singleton
class UploadFilesController @Inject() (
  val jcc: JourneyControllerComponents,
  uploadDocumentsConnector: UploadDocumentsConnector,
  uploadDocumentsConfig: UploadDocumentsConfig
)(implicit val ec: ExecutionContext)
    extends RejectedGoodsSingleJourneyBaseController {

  final val selfUrl: String = jcc.servicesConfig.getString("self.url")

  final val selectDocumentTypePageAction: Call = routes.ChooseFileTypeController.show()
  final val callbackAction: Call               = routes.UploadFilesController.submit()

  final def documentTypeDescription(dt: UploadDocumentType)(implicit request: Request[_]): String =
    implicitly[Messages].apply(s"choose-file-type.file-type.${UploadDocumentType.keyOf(dt)}")

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.selectedDocumentType match {
      case None =>
        Redirect(selectDocumentTypePageAction).asFuture

      case Some(documentType) =>
        val continueUrl =
          if (journey.answers.checkYourAnswersAndChangeMode)
            selfUrl + checkYourAnswers.url
          else
            selfUrl + selectDocumentTypePageAction.url

        uploadDocumentsConnector
          .initialize(
            UploadDocumentsConnector
              .Request(
                UploadDocumentsSessionConfig(
                  nonce = journey.answers.nonce,
                  continueUrl = continueUrl,
                  backlinkUrl = continueUrl,
                  callbackUrl = uploadDocumentsConfig.callbackUrlPrefix + callbackAction.url,
                  cargo = documentType,
                  newFileDescription = Some(documentTypeDescription(documentType))
                ),
                journey.answers.supportingEvidences
                  .map(file => file.copy(description = file.documentType.map(documentTypeDescription _)))
              )
          )
          .map {
            case Some(url) =>
              Redirect(s"${uploadDocumentsConfig.publicUrl}$url")
            case None      =>
              Redirect(
                s"${uploadDocumentsConfig.publicUrl}${uploadDocumentsConfig.contextPath}"
              )
          }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final val submit: Action[AnyContent] = simpleActionReadWriteJourney(
    { implicit request => journey =>
      request.asInstanceOf[Request[AnyContent]].body.asJson.map(_.as[UploadDocumentsCallback]) match {
        case None =>
          logger.warn("missing or invalid callback payload")
          (journey, BadRequest("missing or invalid callback payload"))

        case Some(callback) =>
          journey
            .receiveUploadedFiles(callback.documentType, callback.nonce, callback.uploadedFiles)
            .fold(
              error => (journey, BadRequest(error)),
              modifiedJourney => (modifiedJourney, NoContent)
            )
      }
    },
    isCallback = true
  )
}
