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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.claim_deleted
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.routes as commonRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext

@Singleton
class ClaimDeletedController @Inject() (
  val sessionStore: SessionCache,
  val controllerComponents: MessagesControllerComponents,
  val claimDeletedPage: claim_deleted,
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val config: Configuration
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthRetrievalsAndSessionDataAction
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  final val show: Action[AnyContent] =
    authenticatedActionWithSessionData(implicit request => Ok(claimDeletedPage()))

  val startNewClaim: Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    updateSession(sessionStore, request)(session => SessionData(verifiedEmail = session.verifiedEmail))
      .map(
        _.fold(
          logAndDisplayError("could not reset the session"),
          _ =>
            Redirect(
              commonRoutes.ChooseClaimTypeController.show
            )
        )
      )
  }

  val redirectToDashboard: Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    updateSession(sessionStore, request)(session => SessionData(verifiedEmail = session.verifiedEmail))
      .map(
        _.fold(
          logAndDisplayError("could not reset the session"),
          _ =>
            Redirect(
              viewConfig.viewUploadUrl
            )
        )
      )
  }
}
