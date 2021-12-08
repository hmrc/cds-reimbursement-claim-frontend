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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

abstract class RejectedGoodsSingleJourneyController(
  cc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with Logging
    with SessionUpdates {

  val authenticatedAction: AuthenticatedAction
  val sessionDataAction: SessionDataAction
  val sessionStore: SessionCache
  val config: Configuration

  def simpleActionReadJourney(body: RejectedGoodsSingleJourney => Result): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Future.successful(
        request.sessionData
          .flatMap(_.rejectedGoodsSingleJourney)
          .map(body)
          .getOrElse(Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start()))
      )
    }

  def actionReadJourney(body: Request[_] => RejectedGoodsSingleJourney => Future[Result]): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData
        .flatMap(_.rejectedGoodsSingleJourney)
        .map(body(request))
        .getOrElse(
          Future.successful(
            Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start())
          )
        )
    }

  def simpleActionReadWriteJourney(
    body: Request[_] => RejectedGoodsSingleJourney => (RejectedGoodsSingleJourney, Result)
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData
        .flatMap(_.rejectedGoodsSingleJourney)
        .map(body(request))
        .map { case (modifiedJourney, result) =>
          updateSession(sessionStore, request)(_.copy(rejectedGoodsSingleJourney = Some(modifiedJourney)))
            .flatMap(_.fold(error => Future.failed(error.toException), _ => Future.successful(result)))
        }
        .getOrElse(
          Future.successful(
            Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start())
          )
        )
    }

  def actionReadWriteJourney(
    body: Request[_] => RejectedGoodsSingleJourney => Future[(RejectedGoodsSingleJourney, Result)]
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData
        .flatMap(_.rejectedGoodsSingleJourney)
        .map(body(request))
        .map(_.flatMap { case (modifiedJourney, result) =>
          updateSession(sessionStore, request)(_.copy(rejectedGoodsSingleJourney = Some(modifiedJourney)))
            .flatMap(_.fold(error => Future.failed(error.toException), _ => Future.successful(result)))
        })
        .getOrElse(
          Future.successful(
            Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start())
          )
        )
    }
}
