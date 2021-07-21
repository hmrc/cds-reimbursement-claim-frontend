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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.schedule

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.upload.{FileUploadController, FileUploadServices}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.ScheduledDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{schedule => pages}

import scala.concurrent.ExecutionContext

@Singleton
class ScheduledDocumentController @Inject() (
  authenticatedAction: AuthenticatedAction,
  sessionDataAction: SessionDataAction,
  upscanService: UpscanService,
  fileUploadServices: FileUploadServices,
  errorHandler: ErrorHandler,
  uploadPage: pages.upload
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents, sessionStore: SessionCache)
    extends FileUploadController(authenticatedAction, sessionDataAction, upscanService, errorHandler, cc)
    with SessionDataExtractor {

  import fileUploadServices._

  implicit val scheduledDocumentExtractor: DraftC285Claim => Option[ScheduledDocument] =
    _.scheduledDocumentAnswer

  def uploadScheduledDocument(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ScheduledDocument] { (_, answer) =>
        initiateUpload(answer)(upscanUpload => Ok(uploadPage(upscanUpload)))
      }
    }
}
