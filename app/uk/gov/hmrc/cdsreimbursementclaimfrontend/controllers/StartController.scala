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

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AuthenticatedUser.NonGovernmentGatewayAuthenticatedUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
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
      (
        request.authenticatedRequest.journeyUserType,
        request.sessionData.journeyStatus
      ) match {
        case (authenticatedUser, _) =>
          handleAuthenticatedUser(authenticatedUser)
      }
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
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) => Ok(weOnlySupportGGPage())
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  val signOutAndRegisterForGG: Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) =>
          Redirect(viewConfig.ggCreateAccountUrl).withNewSession
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  val signOutAndSignIn: Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) =>
          Redirect(routes.StartController.start()).withNewSession
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  val keepAlive: Action[AnyContent] =
    authenticatedActionWithSessionData(_ => Ok(""))

  val timedOut: Action[AnyContent] =
    Action(implicit request => Ok(timedOutPage()))

  private def handleAuthenticatedUser(
    authenticatedUser: AuthenticatedUser
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    authenticatedUser match {
      case AuthenticatedUser.Individual(ggCredId, _, eori, _) =>
        handleSignedInUser(ggCredId, eori)

      case AuthenticatedUser.Organisation(ggCredId, _, eori, _) =>
        handleSignedInUser(ggCredId, eori)

      case u: AuthenticatedUser.NonGovernmentGatewayAuthenticatedUser =>
        handleNonGovernmentGatewayUser(u)
    }

  private def handleNonGovernmentGatewayUser(
    nonGovernmentGatewayUser: NonGovernmentGatewayAuthenticatedUser
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    logger.warn(
      s"User logged in with unsupported provider: ${nonGovernmentGatewayUser.authProvider}"
    )

    updateSession(sessionStore, request)(_ =>
      SessionData.empty.copy(
        journeyStatus = Some(NonGovernmentGatewayJourney)
      )
    ).map {
      case Left(e) =>
        logAndDisplayError("could not update session:").apply(e)

      case Right(_) =>
        Redirect(routes.StartController.weOnlySupportGG())
    }

  }

  private def handleSignedInUser(
    ggCredId: GGCredId,
    eori: Eori
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      _ <- EitherT(
             updateSession(sessionStore, request)(_ =>
               SessionData.empty.copy(
                 journeyStatus = Some(
                   FillingOutClaim(
                     ggCredId,
                     request.signedInUserDetailsFromRequest(eori),
                     DraftClaim.blank
                   )
                 )
               )
             )
           )
    } yield ()

    result.fold(
      logAndDisplayError("could not initiate claim journey:"),
      _ =>
        Redirect(
          controllers.common.routes.CheckEoriDetailsController.show()
        )
    )
  }

}
