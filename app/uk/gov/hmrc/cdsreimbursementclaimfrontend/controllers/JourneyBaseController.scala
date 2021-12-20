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

import cats.syntax.eq._
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionDataAndRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.data.Form

/** Base journey controller providing common action behaviours:
  *  - feature switch check
  *  - user authorization
  *  - user data retrieval
  *  - sesion data retrieval and journey update
  *  - journey completeness check and redirect to the CYA page
  */
abstract class JourneyBaseController[Journey](implicit ec: ExecutionContext)
    extends FrontendBaseController
    with Logging {

  /** [Inject] Component expected to be injected by the implementing controller. */
  val jcc: JourneyControllerComponents

  implicit def messages(implicit request: Request[_]): Messages =
    jcc.controllerComponents.messagesApi.preferred(request)

  /** [Config] Defines where to redirect when missing journey or missing session data after user has been authorized. */
  val startOfTheJourney: Call

  private lazy val goToTheStartOfJourney: Future[Result] =
    Future.successful(Redirect(startOfTheJourney))

  /** [Config] Defines where to redirect when journey is already complete, a.k.a CYA page. */
  val checkYourAnswers: Call

  /** [Config] Required feature flag or none. */
  val requiredFeature: Option[Feature]

  def getJourney(sessionData: SessionData): Option[Journey]
  def updateJourney(sessionData: SessionData, journey: Journey): SessionData
  def isComplete(journey: Journey): Boolean

  final override def controllerComponents: MessagesControllerComponents =
    jcc.controllerComponents

  private def resultOrRedirectToCheckYourAnswersIfComplete(result: Result, journey: Journey): Future[Result] =
    Future.successful(
      if (isComplete(journey)) Redirect(checkYourAnswers)
      else result
    )

  private def storeSessionIfChanged(sessionData: SessionData, modifiedSessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]] =
    if (modifiedSessionData === sessionData) Future.successful(Right(()))
    else jcc.sessionCache.store(modifiedSessionData)

  /** Simple GET action to show page based on the journey state */
  final def simpleActionReadJourney(body: Journey => Result): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        Future.successful(
          request.sessionData
            .flatMap(getJourney)
            .map(body)
            .getOrElse(Redirect(startOfTheJourney))
        )
      }

  /** Simple GET action to show page based on the user data and journey state. */
  final def simpleActionReadJourneyAndUser(body: Journey => RetrievedUserType => Result): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        Future.successful(
          getJourney(request.sessionData)
            .map(journey => body(journey)(request.authenticatedRequest.journeyUserType))
            .getOrElse(Redirect(startOfTheJourney))
        )
      }

  /** Async GET action to show page based on the request and journey state. */
  final def actionReadJourney(
    body: Request[_] => Journey => Future[Result]
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getJourney)
          .map(body(request))
          .getOrElse(goToTheStartOfJourney)
      }

  /** Simple POST action to submit form and update journey. */
  final def simpleActionReadWriteJourney(
    body: Request[_] => Journey => (Journey, Result)
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map(body(request))
              .map { case (modifiedJourney, result) =>
                storeSessionIfChanged(sessionData, updateJourney(sessionData, modifiedJourney))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrRedirectToCheckYourAnswersIfComplete(result, modifiedJourney)
                    )
                  )
              }
          )
          .getOrElse(goToTheStartOfJourney)
      }

  /** Simple POST to submit form and update journey, can use current user data. */
  final def simpleActionReadWriteJourneyAndUser(
    body: RequestWithSessionDataAndRetrievedData[_] => Journey => RetrievedUserType => (Journey, Result)
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getJourney(request.sessionData)
          .map(journey => body(request)(journey)(request.authenticatedRequest.journeyUserType))
          .map { case (modifiedJourney, result) =>
            storeSessionIfChanged(request.sessionData, updateJourney(request.sessionData, modifiedJourney))
              .flatMap(
                _.fold(
                  error => Future.failed(error.toException),
                  _ => resultOrRedirectToCheckYourAnswersIfComplete(result, modifiedJourney)
                )
              )
          }
          .getOrElse(goToTheStartOfJourney)
      }

  /** Async POST action to submit form and update journey. */
  final def actionReadWriteJourney(
    body: Request[_] => Journey => Future[(Journey, Result)]
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map(body(request))
              .map(_.flatMap { case (modifiedJourney, result) =>
                storeSessionIfChanged(sessionData, updateJourney(sessionData, modifiedJourney))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrRedirectToCheckYourAnswersIfComplete(result, modifiedJourney)
                    )
                  )
              })
          )
          .getOrElse(goToTheStartOfJourney)

      }

  /** Async POST action to submit form and update journey, can use current user data. */
  final def actionReadWriteJourneyAndUser(
    body: RequestWithSessionDataAndRetrievedData[_] => Journey => RetrievedUserType => Future[(Journey, Result)]
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getJourney(request.sessionData)
          .fold(goToTheStartOfJourney) { journey =>
            body(request)(journey)(request.authenticatedRequest.journeyUserType)
              .flatMap { case (modifiedJourney, result) =>
                storeSessionIfChanged(request.sessionData, updateJourney(request.sessionData, modifiedJourney))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrRedirectToCheckYourAnswersIfComplete(result, modifiedJourney)
                    )
                  )
              }
          }
      }

  implicit class FormOps[A](val form: Form[A]) {
    def withDefault(optValue: Option[A]): Form[A] =
      optValue.map(form.fill).getOrElse(form)
  }
}
