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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

abstract class JourneyBaseController[Journey](
  cc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  val sessionStore: SessionCache

  def getJourney(sessionData: SessionData): Option[Journey]
  def updateJourney(journey: Journey)(sessionData: SessionData): SessionData

  final def simpleActionReadJourney(body: Journey => Result): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Future.successful(
        request.sessionData
          .flatMap(getJourney)
          .map(body)
          .getOrElse(Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start()))
      )
    }

  final def actionReadJourney(body: Request[_] => Journey => Future[Result]): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData
        .flatMap(getJourney)
        .map(body(request))
        .getOrElse(
          Future.successful(
            Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start())
          )
        )
    }

  final def simpleActionReadWriteJourney(
    body: Request[_] => Journey => (Journey, Result)
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData
        .flatMap(getJourney)
        .map(body(request))
        .map { case (modifiedJourney, result) =>
          updateSession(sessionStore, request)(updateJourney(modifiedJourney))
            .flatMap(_.fold(error => Future.failed(error.toException), _ => Future.successful(result)))
        }
        .getOrElse(
          Future.successful(
            Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start())
          )
        )
    }

  final def actionReadWriteJourney(
    body: Request[_] => Journey => Future[(Journey, Result)]
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData
        .flatMap(getJourney)
        .map(body(request))
        .map(_.flatMap { case (modifiedJourney, result) =>
          updateSession(sessionStore, request)(updateJourney(modifiedJourney))
            .flatMap(_.fold(error => Future.failed(error.toException), _ => Future.successful(result)))
        })
        .getOrElse(
          Future.successful(
            Redirect(uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start())
          )
        )
    }
}
