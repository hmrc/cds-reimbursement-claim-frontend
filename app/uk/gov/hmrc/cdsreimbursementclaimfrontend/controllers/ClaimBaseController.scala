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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.CommonClaimProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
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

/** Base claim controller providing common action behaviours:
  *   - feature switch check
  *   - user authorization
  *   - user data retrieval
  *   - sesion data retrieval and claim update
  *   - claim completeness check and redirect to the CYA page
  */
trait ClaimBaseController extends FrontendBaseController with Logging with SeqUtils {

  type Claim <: uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.Claim & ClaimBase & CommonClaimProperties

  implicit def ec: ExecutionContext
  implicit def viewConfig: ViewConfig

  /** [Inject] Component expected to be injected by the implementing controller. */
  val jcc: ClaimControllerComponents

  final override def controllerComponents: MessagesControllerComponents =
    jcc.controllerComponents

  implicit def messages(implicit request: Request[?]): Messages =
    jcc.controllerComponents.messagesApi.preferred(request)

  /** [Config] Defines where to redirect when missing claim or missing session data after user has been authorized. */
  val startOfTheClaim: Call

  final lazy val redirectToTheStartOfTheClaim: Future[Result] = {
    logger.warn(MISSING_CLAIM_DATA_LOG_MESSAGE)
    Future.successful(Redirect(startOfTheClaim))
  }

  /** [Config] Defines where to redirect when claim is already complete, a.k.a CYA page. */
  val checkYourAnswers: Call

  /** [Config] Defines where to redirect when claim has been already finalized. */
  val claimSubmissionConfirmation: Call

  /** [Config] Provides navigation after claim validation failure. */
  def routeForValidationErrors(errors: Seq[String]): Call

  /** Returns state of the claim for the current user. */
  def getClaim(sessionData: SessionData): Option[Claim]

  /** Updates the state of the claim for the current user. */
  def updateClaim(sessionData: SessionData, claim: Claim): SessionData

  /** Optional claim access precondition. */
  def claimAccessPrecondition(implicit request: Request[?]): Option[Validate[Claim]] = None

  /** Optional action precondition. */
  val actionPrecondition: Option[Validate[Claim]] = None

  private val MISSING_CLAIM_DATA_LOG_MESSAGE =
    "Missing claim data in session, redirecting to the start page."

  /** Check if claim access precondition met when defined, and if not then return the list of errors. */
  private final def checkIfMaybeClaimAccessPreconditionFails(claim: Claim)(implicit
    request: Request[?]
  ): Option[Seq[String]] =
    claimAccessPrecondition
      .flatMap(
        _.apply(claim).fold(
          errors => {
            logger.warn(s"Claim access preconditions not met: ${errors.messages.mkString(",")}")
            Some(errors.messages)
          },
          _ => None
        )
      )

  /** Check if action precondition met when defined, and if not then return the list of errors. */
  final def checkIfMaybeActionPreconditionFails(claim: Claim)(implicit
    request: Request[?]
  ): Option[Seq[String]] =
    checkIfMaybeClaimAccessPreconditionFails(claim)
      .orElse(
        actionPrecondition
          .flatMap(
            _.apply(claim).fold(
              errors => {
                logger.warn(s"Action preconditions not met: ${errors.messages.mkString(",")}")
                Some(errors.messages)
              },
              _ => None
            )
          )
      )

  /** Check if the CYA page should be displayed next. */
  final def shouldForwardToCYA(claim: Claim): Boolean =
    claim.userHasSeenCYAPage && claim.hasCompleteAnswers

  private final def resultOrShortcut(
    result: Result,
    claim: Claim,
    fastForwardToCYAEnabled: Boolean
  ): Future[Result] =
    Future.successful(
      if result.header.status =!= 303 then result
      else if claim.isFinalized then Redirect(claimSubmissionConfirmation)
      else if fastForwardToCYAEnabled && shouldForwardToCYA(claim) then Redirect(checkYourAnswers)
      else result
    )

  final def storeSessionIfChanged(sessionData: SessionData, modifiedSessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]] =
    if modifiedSessionData === sessionData then Future.successful(Right(()))
    else jcc.sessionCache.store(modifiedSessionData)

  /** Simple GET action to show page based on the current claim state. */
  final def simpleActionReadClaim(body: Claim => Result): Action[AnyContent] =
    jcc.authenticatedActionWithSessionData
      .async { implicit request =>
        Future.successful(
          request.sessionData
            .flatMap(getClaim)
            .map((claim: Claim) =>
              if claim.isFinalized then Redirect(claimSubmissionConfirmation)
              else
                checkIfMaybeActionPreconditionFails(claim) match {
                  case None         => body(claim)
                  case Some(errors) => Redirect(routeForValidationErrors(errors))
                }
            )
            .getOrElse {
              logger.warn(MISSING_CLAIM_DATA_LOG_MESSAGE)
              Redirect(startOfTheClaim)
            }
        )
      }

  /** Async GET action to show page based on the request and the current claim state. */
  final def actionReadClaim(
    body: Request[?] => Claim => Result | Future[Result]
  ): Action[AnyContent] =
    jcc.authenticatedActionWithSessionData
      .async { implicit request =>
        request.sessionData
          .flatMap(getClaim)
          .map((claim: Claim) =>
            if claim.isFinalized then Future.successful(Redirect(claimSubmissionConfirmation))
            else
              checkIfMaybeActionPreconditionFails(claim) match {
                case None         => body(request)(claim).toFuture
                case Some(errors) => Future.successful(Redirect(routeForValidationErrors(errors)))
              }
          )
          .getOrElse(redirectToTheStartOfTheClaim)
      }

  /** Simple POST action to submit form and update the current claim state. */
  final def simpleActionReadWriteClaim(
    body: Request[?] => Claim => (Claim, Result),
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc.authenticatedActionWithSessionData
      .async { implicit request =>
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)
        request.sessionData
          .flatMap(sessionData =>
            getClaim(sessionData)
              .map((claim: Claim) =>
                if claim.isFinalized then (claim, Redirect(claimSubmissionConfirmation))
                else
                  checkIfMaybeActionPreconditionFails(claim) match {
                    case None         => body(request)(claim)
                    case Some(errors) =>
                      logger.info(s"${errors.headOption}, ${routeForValidationErrors(errors)}")
                      (claim, Redirect(routeForValidationErrors(errors)))
                  }
              )
              .map { case (modifiedClaim, result) =>
                storeSessionIfChanged(sessionData, updateClaim(sessionData, modifiedClaim))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrShortcut(result, modifiedClaim, fastForwardToCYAEnabled)
                    )
                  )
              }
          )
          .getOrElse(redirectToTheStartOfTheClaim)
      }

  /** Simple POST action to submit form and update the current claim state. */
  final def simpleActionReadWriteClaimWhenCallback(
    body: Request[?] => Claim => (Claim, Result),
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc.authenticatedActionWithSessionDataWhenCallback
      .async { implicit request =>
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)
        request.sessionData
          .flatMap(sessionData =>
            getClaim(sessionData)
              .map((claim: Claim) =>
                if claim.isFinalized then (claim, Redirect(claimSubmissionConfirmation))
                else
                  checkIfMaybeActionPreconditionFails(claim) match {
                    case None         => body(request)(claim)
                    case Some(errors) =>
                      logger.info(s"${errors.headOption}, ${routeForValidationErrors(errors)}")
                      (claim, Redirect(routeForValidationErrors(errors)))
                  }
              )
              .map { case (modifiedClaim, result) =>
                storeSessionIfChanged(sessionData, updateClaim(sessionData, modifiedClaim))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrShortcut(result, modifiedClaim, fastForwardToCYAEnabled)
                    )
                  )
              }
          )
          .getOrElse(redirectToTheStartOfTheClaim)
      }

  /** Async POST action to submit form and update claim. */
  final def actionReadWriteClaim(
    body: Request[?] => Claim => (Claim, Result) | Future[(Claim, Result)],
    fastForwardToCYAEnabled: Boolean = true
  ): Action[AnyContent] =
    jcc.authenticatedActionWithSessionData
      .async { implicit request =>
        request.sessionData
          .flatMap(sessionData =>
            getClaim(sessionData)
              .map((claim: Claim) =>
                if claim.isFinalized then Future.successful((claim, Redirect(claimSubmissionConfirmation)))
                else
                  checkIfMaybeActionPreconditionFails(claim) match {
                    case None         => body(request)(claim)
                    case Some(errors) => Future.successful((claim, Redirect(routeForValidationErrors(errors))))
                  }
              )
              .map(_.flatMap { case (modifiedClaim, result) =>
                storeSessionIfChanged(sessionData, updateClaim(sessionData, modifiedClaim))
                  .flatMap(
                    _.fold(
                      error => Future.failed(error.toException),
                      _ => resultOrShortcut(result, modifiedClaim, fastForwardToCYAEnabled)
                    )
                  )
              })
          )
          .getOrElse(redirectToTheStartOfTheClaim)

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
  }

  final def prettyPrint(claim: Claim)(implicit format: Format[Claim]): String =
    Json.prettyPrint(Json.toJson(claim))

  final def logAndDisplayError(
    description: String,
    errors: String*
  )(implicit errorHandler: ErrorHandler, request: Request[?]): Result = {
    import errorHandler._
    logger.error(s"$description${if errors.nonEmpty then errors.mkString(": ", ", ", "") else ""}")
    errorResult()
  }

  extension (result: (Claim, Result) | Future[(Claim, Result)]) {
    inline def flatMap[S](f: ((Claim, Result)) => Future[S]): Future[S] =
      result match {
        case future: Future[(Claim, Result)] => future.flatMap(f)
        case value: (Claim, Result)          => f(value)
      }
  }

  extension (result: Result | Future[Result]) {
    inline def toFuture: Future[Result] =
      result match {
        case future: Future[?] => future.asInstanceOf[Future[Result]]
        case value: Result     => Future.successful(value)
      }
  }

}
