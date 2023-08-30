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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class StartController @Inject() (
  val sessionStore: SessionCache,
  val controllerComponents: MessagesControllerComponents,
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val config: Configuration,
  weOnlySupportGGPage: views.html.we_only_support_gg,
  timedOutPage: views.html.timed_out
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthRetrievalsAndSessionDataAction
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  val start: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.whenAuthorisedUser((_, _) =>
        Future.successful(
          Redirect(
            controllers.common.routes.CheckEoriDetailsController.show()
          )
        )
      )(resultIfUnsupportedUser = Redirect(routes.StartController.weOnlySupportGG()))
    }

  val startNewClaim: Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    updateSession(sessionStore, request)(_ => SessionData.empty)
      .map(
        _.fold(
          logAndDisplayError("could not reset the session"),
          _ =>
            Redirect(
              controllers.routes.StartController.start()
            )
        )
      )
  }

  val weOnlySupportGG: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.whenAuthorisedUser { (_, _) =>
        Future.successful(Redirect(routes.StartController.start()))
      }(resultIfUnsupportedUser = Ok(weOnlySupportGGPage()))
    }

  val signOutAndRegisterForGG: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.whenAuthorisedUser { (_, _) =>
        Future.successful(Redirect(routes.StartController.start()))
      }(resultIfUnsupportedUser = Redirect(viewConfig.ggCreateAccountUrl).withNewSession)
    }

  val signOutAndSignIn: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.whenAuthorisedUser { (_, _) =>
        Future.successful(Redirect(routes.StartController.start()))
      }(resultIfUnsupportedUser = Redirect(routes.StartController.start()).withNewSession)
    }

  val keepAlive: Action[AnyContent] =
    authenticatedActionWithSessionData(_ => Ok(""))

  val timedOut: Action[AnyContent] =
    Action(implicit request => Ok(timedOutPage()))

}
