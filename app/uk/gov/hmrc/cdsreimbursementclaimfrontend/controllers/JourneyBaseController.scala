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

import cats.syntax.eq.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.data.Form
import play.api.i18n.Messages
import play.api.libs.json.Format
import play.api.libs.json.Json
import play.api.mvc.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementWithCorrectAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/** Base journey controller providing common action behaviours:
  *   - feature switch check
  *   - user authorization
  *   - user data retrieval
  *   - sesion data retrieval and journey update
  *   - journey completeness check and redirect to the CYA page
  */
trait JourneyBaseController extends FrontendBaseController with Logging with SeqUtils {

  type Journey <: uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.Journey & JourneyBase & CommonJourneyProperties

  implicit def ec: ExecutionContext
  implicit def viewConfig: ViewConfig

  /** [Inject] Component expected to be injected by the implementing controller. */
  val jcc: JourneyControllerComponents

  final override def controllerComponents: MessagesControllerComponents =
    jcc.controllerComponents

  implicit def messages(implicit request: Request[?]): Messages =
    jcc.controllerComponents.messagesApi.preferred(request)

  /** [Config] Defines where to redirect when missing journey or missing session data after user has been authorized. */
  val startOfTheJourney: Call

  final lazy val redirectToTheStartOfTheJourney: Future[Result] = {
    logger.warn(MISSING_JOURNEY_DATA_LOG_MESSAGE)
    Future.successful(Redirect(startOfTheJourney))
  }

  /** [Config] Defines where to redirect when journey is already complete, a.k.a CYA page. */
  val checkYourAnswers: Call

  /** [Config] Defines where to redirect when journey has been already finalized. */
  val claimSubmissionConfirmation: Call

  /** [Config] Required feature flag or none. */
  val requiredFeature: Option[Feature] = None

  /** [Config] Provides navigation after journey validation failure. */
  def routeForValidationErrors(errors: Seq[String]): Call

  /** Returns state of the journey for the current user. */
  def getJourney(sessionData: SessionData): Option[Journey]

  /** Updates the state of the journey for the current user. */
  def updateJourney(sessionData: SessionData, journey: Journey): SessionData

  /** Optional journey access precondition. */
  def journeyAccessPrecondition(implicit request: Request[?]): Option[Validate[Journey]] = None

  /** Optional action precondition. */
  val actionPrecondition: Option[Validate[Journey]] = None

  private val MISSING_JOURNEY_DATA_LOG_MESSAGE =
    "Missing journey data in session, redirecting to the start page."

  /** Check if journey access precondition met when defined, and if not then return the list of errors. */
  private final def checkIfMaybeJourneyAccessPreconditionFails(journey: Journey)(implicit
    request: Request[?]
  ): Option[Seq[String]] =
    journeyAccessPrecondition
      .flatMap(
        _.apply(journey).fold(
          errors => {
            logger.warn(s"Journey access preconditions not met: ${errors.messages.mkString(",")}")
            Some(errors.messages)
          },
          _ => None
        )
      )

  /** Check if action precondition met when defined, and if not then return the list of errors. */
  final def checkIfMaybeActionPreconditionFails(journey: Journey)(implicit
    request: Request[?]
  ): Option[Seq[String]] =
    checkIfMaybeJourneyAccessPreconditionFails(journey)
      .orElse(
        actionPrecondition
          .flatMap(
            _.apply(journey).fold(
              errors => {
                logger.warn(s"Action preconditions not met: ${errors.messages.mkString(",")}")
                Some(errors.messages)
              },
              _ => None
            )
          )
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
      if result.header.status =!= 303 then result
      else if journey.isFinalized then Redirect(claimSubmissionConfirmation)
      else if fastForwardToCYAEnabled && shouldForwardToCYA(journey) then Redirect(checkYourAnswers)
      else result
    )

  final def storeSessionIfChanged(sessionData: SessionData, modifiedSessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]] =
    if modifiedSessionData === sessionData then Future.successful(Right(()))
    else jcc.sessionCache.store(modifiedSessionData)

  /** Simple GET action to show page based on the current journey state. */
  final def simpleActionReadJourney(body: Journey => Result): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        Future.successful(
          request.sessionData
            .flatMap(getJourney)
            .map((journey: Journey) =>
              if journey.isFinalized then Redirect(claimSubmissionConfirmation)
              else
                checkIfMaybeActionPreconditionFails(journey) match {
                  case None         => body(journey)
                  case Some(errors) => Redirect(routeForValidationErrors(errors))
                }
            )
            .getOrElse {
              logger.warn(MISSING_JOURNEY_DATA_LOG_MESSAGE)
              Redirect(startOfTheJourney)
            }
        )
      }

  /** Async GET action to show page based on the request and the current journey state. */
  final def actionReadJourney(
    body: Request[?] => Journey => Future[Result]
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getJourney)
          .map((journey: Journey) =>
            if journey.isFinalized then Future.successful(Redirect(claimSubmissionConfirmation))
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
    body: Request[?] => Journey => (Journey, Result),
    isCallback: Boolean = false,
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature, isCallback)
      .async { implicit request =>
        implicit val hc: HeaderCarrier =
          if isCallback then HeaderCarrierConverter.fromRequest(request)
          else HeaderCarrierConverter.fromRequestAndSession(request, request.session)
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map((journey: Journey) =>
                if journey.isFinalized then (journey, Redirect(claimSubmissionConfirmation))
                else
                  checkIfMaybeActionPreconditionFails(journey) match {
                    case None         => body(request)(journey)
                    case Some(errors) =>
                      logger.info(s"${errors.headOption}, ${routeForValidationErrors(errors)}")
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

  /** Async POST action to submit form and update journey. */
  final def actionReadWriteJourney(
    body: Request[?] => Journey => Future[(Journey, Result)],
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getJourney(sessionData)
              .map((journey: Journey) =>
                if journey.isFinalized then Future.successful((journey, Redirect(claimSubmissionConfirmation)))
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

  final def getReimbursementWithCorrectAmount(
    reimbursements: Seq[Reimbursement]
  ): Seq[ReimbursementWithCorrectAmount] =
    reimbursements.map { reimbursement =>
      ReimbursementWithCorrectAmount(
        reimbursement.taxCode,
        reimbursement.amount,
        reimbursement.paidAmount,
        reimbursement.correctedAmount.getOrElse(BigDecimal(0))
      )
    }

  implicit class FormOps[A](val form: Form[A]) {
    def withDefault(optValue: Option[A]): Form[A] =
      optValue.map(form.fill).getOrElse(form)
  }

  implicit class Ops[A](val value: A) {
    def asFuture: Future[A] = Future.successful(value)
    def |>[B](f: A => B): B = f(value)
  }

  final def prettyPrint(journey: Journey)(implicit format: Format[Journey]): String =
    Json.prettyPrint(Json.toJson(journey))

  final def logAndDisplayError(
    description: String,
    errors: String*
  )(implicit errorHandler: ErrorHandler, request: Request[?]): Result = {
    import errorHandler._
    logger.error(s"$description${if errors.nonEmpty then errors.mkString(": ", ", ", "") else ""}")
    errorResult()
  }

}
