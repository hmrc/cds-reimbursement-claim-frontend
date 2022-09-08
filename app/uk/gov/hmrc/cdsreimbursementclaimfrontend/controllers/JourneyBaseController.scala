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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.syntax.eq._
import play.api.data.Form
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
import play.api.libs.json.Format
import play.api.libs.json.Json

import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase

/** Base journey controller providing common action behaviours:
  *  - feature switch check
  *  - user authorization
  *  - user data retrieval
  *  - sesion data retrieval and journey update
  *  - journey completeness check and redirect to the CYA page
  */
abstract class JourneyBaseController[Journey <: JourneyBase[Journey]](implicit
  ec: ExecutionContext,
  fmt: Format[Journey]
) extends FrontendBaseController
    with Logging
    with SeqUtils {

  /** [Inject] Component expected to be injected by the implementing controller. */
  val jcc: JourneyControllerComponents

  final override def controllerComponents: MessagesControllerComponents =
    jcc.controllerComponents

  implicit def messages(implicit request: Request[_]): Messages =
    jcc.controllerComponents.messagesApi.preferred(request)

  /** [Config] Defines where to redirect when missing journey or missing session data after user has been authorized. */
  val startOfTheJourney: Call

  final lazy val redirectToTheStartOfTheJourney: Future[Result] =
    Future.successful(Redirect(startOfTheJourney))

  /** [Config] Defines where to redirect when journey is already complete, a.k.a CYA page. */
  val checkYourAnswers: Call

  /** [Config] Defines where to redirect when journey has been already finalized. */
  val claimSubmissionConfirmation: Call

  /** [Config] Required feature flag or none. */
  val requiredFeature: Option[Feature]

  /** [Config] Provides navigation after journey validation failure. */
  def routeForValidationErrors(errors: Seq[String]): Call

  /** Returns state of the journey for the current user. */
  def getJourney(sessionData: SessionData): Option[Journey]

  /** Updates the state of the journey for the current user. */
  def updateJourney(sessionData: SessionData, journey: Journey): SessionData

  /** Optional action precondition. */
  val actionPrecondition: Option[Validate[Journey]] = None

  /** Check if action precondition met when defined, and if not then return the list of errors. */
  final def checkIfMaybeActionPreconditionFails(journey: Journey): Option[Seq[String]] =
    actionPrecondition.fold[Option[Seq[String]]](None)(
      _.apply(journey).fold(errors => Some(errors.messages), _ => None)
    )

  /** Check if the CYA page should be displayed next. */
  final def shouldForwardToCYA(journey: Journey): Boolean =
    journey.userHasSeenCYAPage && journey.hasCompleteAnswers

  private final def resultOrShortcut(
    result: Result,
    journey: Journey,
    fastForwardToCYAEnabled: Boolean
  ): Future[Result] =
    Future.successful(
      if (result.header.status =!= 303) result
      else if (journey.isFinalized) Redirect(claimSubmissionConfirmation)
      else if (fastForwardToCYAEnabled && shouldForwardToCYA(journey))
        Redirect(checkYourAnswers)
      else result
    )

  final def storeSessionIfChanged(sessionData: SessionData, modifiedSessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]] =
    if (modifiedSessionData === sessionData) Future.successful(Right(()))
    else jcc.sessionCache.store(modifiedSessionData)

  /** Simple GET action to show page based on the current journey state. */
  final def simpleActionReadJourney(body: Journey => Result): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        Future.successful(
          request.sessionData
            .flatMap(getJourney)
            .map(journey =>
              if (journey.isFinalized) Redirect(claimSubmissionConfirmation)
              else
                checkIfMaybeActionPreconditionFails(journey) match {
                  case None         => body(journey)
                  case Some(errors) => Redirect(routeForValidationErrors(errors))
                }
            )
            .getOrElse(Redirect(startOfTheJourney))
        )
      }

  /** Async GET action to show page based on the current user's auth data and journey state. */
  final def actionReadJourneyAndUser(
    body: Request[_] => Journey => RetrievedUserType => Future[Result]
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getJourney(request.sessionData)
          .map(journey =>
            if (journey.isFinalized) Future.successful(Redirect(claimSubmissionConfirmation))
            else
              checkIfMaybeActionPreconditionFails(journey) match {
                case None         => body(request)(journey)(request.authenticatedRequest.journeyUserType)
                case Some(errors) => Future.successful(Redirect(routeForValidationErrors(errors)))
              }
          )
          .getOrElse(redirectToTheStartOfTheJourney)
      }

  /** Simple GET action to show page based on the current user's auth data and journey state. */
  final def simpleActionReadJourneyAndUser(
    body: Journey => RetrievedUserType => Result
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .apply { implicit request =>
        getJourney(request.sessionData)
          .map(journey =>
            if (journey.isFinalized) Redirect(claimSubmissionConfirmation)
            else
              checkIfMaybeActionPreconditionFails(journey) match {
                case None         => body(journey)(request.authenticatedRequest.journeyUserType)
                case Some(errors) => Redirect(routeForValidationErrors(errors))
              }
          )
          .getOrElse(Redirect(startOfTheJourney))
      }

  /** Async GET action to show page based on the request and the current journey state. */
  final def actionReadJourney(
    body: Request[_] => Journey => Future[Result]
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getJourney)
          .map(journey =>
            if (journey.isFinalized) Future.successful(Redirect(claimSubmissionConfirmation))
            else
              checkIfMaybeActionPreconditionFails(journey) match {
                case None         => body(request)(journey)
                case Some(errors) => Future.successful(Redirect(routeForValidationErrors(errors)))
              }
          )
          .getOrElse(redirectToTheStartOfTheJourney)
      }

  /** Simple POST action to submit form and update the current journey state. */
  final def simpleActionReadWriteJourney(
    body: Request[_] => Journey => (Journey, Result),
    isCallback: Boolean = false,
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature, isCallback)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map(journey =>
                if (journey.isFinalized) (journey, Redirect(claimSubmissionConfirmation))
                else
                  checkIfMaybeActionPreconditionFails(journey) match {
                    case None         => body(request)(journey)
                    case Some(errors) =>
                      println((errors.headOption, routeForValidationErrors(errors)))
                      (journey, Redirect(routeForValidationErrors(errors)))
                  }
              )
              .map { case (modifiedJourney, result) =>
                storeSessionIfChanged(sessionData, updateJourney(sessionData, modifiedJourney))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrShortcut(result, modifiedJourney, fastForwardToCYAEnabled)
                    )
                  )
              }
          )
          .getOrElse(redirectToTheStartOfTheJourney)
      }

  /** Simple POST to submit form and update the current journey, can use current user's auth data. */
  final def simpleActionReadWriteJourneyAndUser(
    body: RequestWithSessionDataAndRetrievedData[_] => Journey => RetrievedUserType => (Journey, Result),
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getJourney(request.sessionData)
          .map(journey =>
            if (journey.isFinalized) (journey, Redirect(claimSubmissionConfirmation))
            else
              checkIfMaybeActionPreconditionFails(journey) match {
                case None         => body(request)(journey)(request.authenticatedRequest.journeyUserType)
                case Some(errors) => (journey, Redirect(routeForValidationErrors(errors)))
              }
          )
          .map { case (modifiedJourney, result) =>
            storeSessionIfChanged(request.sessionData, updateJourney(request.sessionData, modifiedJourney))
              .flatMap(
                _.fold(
                  error => Future.failed(error.toException),
                  _ => resultOrShortcut(result, modifiedJourney, fastForwardToCYAEnabled)
                )
              )
          }
          .getOrElse(redirectToTheStartOfTheJourney)
      }

  /** Async POST action to submit form and update journey. */
  final def actionReadWriteJourney(
    body: Request[_] => Journey => Future[(Journey, Result)],
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map(journey =>
                if (journey.isFinalized) Future.successful((journey, Redirect(claimSubmissionConfirmation)))
                else
                  checkIfMaybeActionPreconditionFails(journey) match {
                    case None         => body(request)(journey)
                    case Some(errors) => Future.successful((journey, Redirect(routeForValidationErrors(errors))))
                  }
              )
              .map(_.flatMap { case (modifiedJourney, result) =>
                storeSessionIfChanged(sessionData, updateJourney(sessionData, modifiedJourney))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrShortcut(result, modifiedJourney, fastForwardToCYAEnabled)
                    )
                  )
              })
          )
          .getOrElse(redirectToTheStartOfTheJourney)

      }

  /** Async POST action to submit form and update the journey, or clear the session. */
  final def actionReadWriteJourneyOrClearSession(
    body: Request[_] => Journey => Future[Either[Result, (Journey, Result)]],
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map(journey =>
                if (journey.isFinalized) Future.successful(Right((journey, Redirect(claimSubmissionConfirmation))))
                else
                  checkIfMaybeActionPreconditionFails(journey) match {
                    case None         => body(request)(journey)
                    case Some(errors) => Future.successful(Right((journey, Redirect(routeForValidationErrors(errors)))))
                  }
              )
              .map(_.flatMap {
                case Right((modifiedJourney, result)) =>
                  storeSessionIfChanged(sessionData, updateJourney(sessionData, modifiedJourney))
                    .flatMap(
                      _.fold(
                        error => Future.failed(error.toException),
                        _ => resultOrShortcut(result, modifiedJourney, fastForwardToCYAEnabled)
                      )
                    )
                case Left(result)                     =>
                  jcc.sessionCache
                    .store(SessionData.empty)
                    .map(_ => result)
              })
          )
          .getOrElse(redirectToTheStartOfTheJourney)

      }

  /** Async POST action to submit form and update the journey, can use the current user's auth data. */
  final def actionReadWriteJourneyAndUser(
    body: RequestWithSessionDataAndRetrievedData[_] => Journey => RetrievedUserType => Future[(Journey, Result)],
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithRetrievedDataAndSessionData(requiredFeature)
      .async { implicit request =>
        getJourney(request.sessionData)
          .fold(redirectToTheStartOfTheJourney) { journey =>
            if (journey.isFinalized) Future.successful(Redirect(claimSubmissionConfirmation))
            else
              (checkIfMaybeActionPreconditionFails(journey) match {
                case None         => body(request)(journey)(request.authenticatedRequest.journeyUserType)
                case Some(errors) => Future.successful((journey, Redirect(routeForValidationErrors(errors))))
              }).flatMap { case (modifiedJourney, result) =>
                storeSessionIfChanged(request.sessionData, updateJourney(request.sessionData, modifiedJourney))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrShortcut(result, modifiedJourney, fastForwardToCYAEnabled)
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

  final def prettyPrint(journey: Journey): String =
    Json.prettyPrint(Json.toJson(journey))

  final def logAndDisplayError(
    description: String,
    errors: String*
  )(implicit errorHandler: ErrorHandler, request: Request[_]): Result = {
    import errorHandler._
    logger.error(s"$description${if (errors.nonEmpty) errors.mkString(": ", ", ", "") else ""}")
    errorResult()
  }

}
