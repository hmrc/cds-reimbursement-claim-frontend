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

import cats.syntax.eq._
import play.api.data.Form
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionDataAndRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AuthenticatedUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.libs.json.Format
import play.api.libs.json.Json

/** Base model controller providing common action behaviours:
  *  - feature switch check
  *  - user authorization
  *  - user data retrieval
  *  - model retrieval and update
  */
abstract class ModelBaseController[Model](implicit ec: ExecutionContext, fmt: Format[Model])
    extends FrontendBaseController
    with Logging {

  /** [Inject] Component expected to be injected by the implementing controller. */
  val ccc: JourneyControllerComponents

  /** [Config] Defines where to redirect when missing model or missing session data after user has been authorized. */
  val fallbackResultIfModelMissing: Result

  /** [Config] Required feature flag or none. */
  val requiredFeature: Option[Feature]

  def getModel(sessionData: SessionData): Option[Model]
  def updateModel(sessionData: SessionData, model: Model): SessionData

  implicit def messages(implicit request: Request[_]): Messages =
    ccc.controllerComponents.messagesApi.preferred(request)

  final override def controllerComponents: MessagesControllerComponents =
    ccc.controllerComponents

  final def storeSessionIfChanged(sessionData: SessionData, modifiedSessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]] =
    if (modifiedSessionData === sessionData) Future.successful(Right(()))
    else ccc.sessionCache.store(modifiedSessionData)

  /** Simple GET action to show page based on the model state */
  final def simpleActionReadModel(body: Model => Result): Action[AnyContent] =
    ccc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        Future.successful(
          request.sessionData
            .flatMap(getModel)
            .map(body)
            .getOrElse(fallbackResultIfModelMissing)
        )
      }

  /** Async GET action to show page based on the user data and model state. */
  final def actionReadModelAndUser(
    body: Request[_] => Model => AuthenticatedUser => Future[Result]
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getModel(request.sessionData)
          .map(model => body(request)(model)(request.authenticatedRequest.journeyUserType))
          .getOrElse(fallbackResultIfModelMissing.asFuture)
      }

  /** Simple GET action to show page based on the user data and model state. */
  final def simpleActionReadModelAndUser(
    body: Model => AuthenticatedUser => Result
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .apply { implicit request =>
        getModel(request.sessionData)
          .map(model => body(model)(request.authenticatedRequest.journeyUserType))
          .getOrElse(fallbackResultIfModelMissing)
      }

  /** Async GET action to show page based on the request and model state. */
  final def actionReadModel(
    body: Request[_] => Model => Future[Result]
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getModel)
          .map(model => body(request)(model))
          .getOrElse(fallbackResultIfModelMissing.asFuture)
      }

  /** Simple POST action to submit form and update model. */
  final def simpleActionReadWriteModel(
    body: Request[_] => Model => (Model, Result),
    isCallback: Boolean = false
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithSessionData(requiredFeature, isCallback)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getModel(sessionData)
              .map(model => body(request)(model))
              .map { case (modifiedModel, result) =>
                storeSessionIfChanged(sessionData, updateModel(sessionData, modifiedModel))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => Future.successful(result)
                    )
                  )
              }
          )
          .getOrElse(fallbackResultIfModelMissing.asFuture)
      }

  /** Simple POST to submit form and update model, can use current user data. */
  final def simpleActionReadWriteModelAndUser(
    body: RequestWithSessionDataAndRetrievedData[_] => Model => AuthenticatedUser => (Model, Result)
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getModel(request.sessionData)
          .map(model => body(request)(model)(request.authenticatedRequest.journeyUserType))
          .map { case (modifiedModel, result) =>
            storeSessionIfChanged(request.sessionData, updateModel(request.sessionData, modifiedModel))
              .flatMap(
                _.fold(
                  error => Future.failed(error.toException),
                  _ => Future.successful(result)
                )
              )
          }
          .getOrElse(fallbackResultIfModelMissing.asFuture)
      }

  /** Async POST action to submit form and replace the model using a new value if not modified. */
  final def actionReadWriteModel(
    body: Request[_] => Model => Future[(Model, Result)]
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getModel(sessionData)
              .map(model => body(request)(model))
              .map(_.flatMap { case (modifiedModel, result) =>
                storeSessionIfChanged(sessionData, updateModel(sessionData, modifiedModel))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => Future.successful(result)
                    )
                  )
              })
          )
          .getOrElse(fallbackResultIfModelMissing.asFuture)

      }

  /** Async POST action to submit form and update the model using a modification function. */
  final def actionReadUpdateModel(
    body: Request[_] => Model => Future[(Model => Model, Result)]
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getModel(sessionData)
              .map(model => body(request)(model))
              .map(_.flatMap { case (modification, result) =>
                ccc.sessionCache
                  .update(s => getModel(s).map(modification).map(m => updateModel(s, m)).getOrElse(s))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => Future.successful(result)
                    )
                  )
              })
          )
          .getOrElse(fallbackResultIfModelMissing.asFuture)

      }

  /** Async POST action to submit form and update model, can use current user data. */
  final def actionReadWriteModelAndUser(
    body: RequestWithSessionDataAndRetrievedData[_] => Model => AuthenticatedUser => Future[(Model, Result)]
  ): Action[AnyContent] =
    ccc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getModel(request.sessionData)
          .fold(fallbackResultIfModelMissing.asFuture) { model =>
            body(request)(model)(request.authenticatedRequest.journeyUserType)
              .flatMap { case (modifiedModel, result) =>
                storeSessionIfChanged(request.sessionData, updateModel(request.sessionData, modifiedModel))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => Future.successful(result)
                    )
                  )
              }
          }
      }

  implicit class FormOps[A](val form: Form[A]) {
    def withDefault(optValue: Option[A]): Form[A] =
      optValue.map(form.fill).getOrElse(form)
  }

  implicit class Ops[A](val value: A) {
    def asFuture: Future[A] = Future.successful(value)
    def |>[B](f: A => B): B = f(value)
  }

  final def prettyPrint(model: Model): String =
    Json.prettyPrint(Json.toJson(model))

}
