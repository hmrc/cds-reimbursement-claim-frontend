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

import com.google.inject.{Inject, Singleton}
import play.api.Logging
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{schedule => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class ScheduleOfMrnDocumentController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val upscanService: UpscanService,
  val errorHandler: ErrorHandler,
  val sessionStore: SessionCache,
  fileUploadHelperInstances: FileUploadHelperInstances,
  uploadPage: pages.upload
)(implicit viewConfig: ViewConfig, executionContext: ExecutionContext, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with FileUploadController
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  import fileUploadHelperInstances._

  implicit val scheduledDocumentExtractor: DraftC285Claim => Option[ScheduledDocumentAnswer] =
    _.scheduledDocumentAnswer

  def uploadScheduledDocument(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ScheduledDocumentAnswer] { (_, answer) =>
        initiateUpload(answer)(upscanUpload => Ok(uploadPage(upscanUpload)))
      }
    }
}
