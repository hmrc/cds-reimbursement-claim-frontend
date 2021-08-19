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

import cats.data.EitherT
import com.google.inject.{Inject, Singleton}
import play.api.i18n.Lang
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult.{SubmitClaimError, SubmitClaimSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim.CompleteC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.{SubmitClaimRequest, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{CompleteClaim, Error, RetrievedUserType, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckYourAnswersAndSubmitController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  claimService: ClaimService,
  checkYourAnswersPage: pages.check_your_answers,
  confirmationOfSubmissionPage: pages.confirmation_of_submission,
  submitClaimFailedPage: pages.submit_claim_error
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def checkAllAnswers(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCompleteDraftClaim(request) { (_, _, completeClaim) =>
        Ok(checkYourAnswersPage(journey, completeClaim))
      }
    }

  def checkAllAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCompleteDraftClaim(request) { (_, fillingOutClaim, completeClaim) =>
        val result =
          for {
            response        <- EitherT.liftF(
                                 submitClaim(
                                   completeClaim,
                                   fillingOutClaim,
                                   fillingOutClaim.signedInUserDetails,
                                   request.authenticatedRequest.request.messages.lang
                                 )
                               )
            newJourneyStatus = response match {
                                 case _: SubmitClaimError =>
                                   SubmitClaimFailed(
                                     fillingOutClaim.ggCredId,
                                     fillingOutClaim.signedInUserDetails
                                   )
                                 case SubmitClaimSuccess(
                                       submitClaimResponse
                                     ) =>
                                   JustSubmittedClaim(
                                     fillingOutClaim.ggCredId,
                                     fillingOutClaim.signedInUserDetails,
                                     completeClaim,
                                     submitClaimResponse
                                   )
                               }
            _               <- EitherT(
                                 updateSession(sessionStore, request)(
                                   _.copy(journeyStatus = Some(newJourneyStatus))
                                 )
                               )
          } yield response

        result.fold(
          logAndDisplayError("Error while trying to update session"),
          {
            case SubmitClaimError(e) =>
              logger.warn(s"Could not submit return}", e)
              Redirect(
                claimsRoutes.CheckYourAnswersAndSubmitController.submissionError()
              )

            case SubmitClaimSuccess(_) =>
              logger.info(
                s"Successfully submitted claim with claim id :${completeClaim.id}"
              )
              Redirect(
                claimsRoutes.CheckYourAnswersAndSubmitController
                  .confirmationOfSubmission()
              )
          }
        )
      }

    }

  private def submitClaim(
    completeClaim: CompleteClaim,
    fillingOutClaim: FillingOutClaim,
    signedInUserDetails: SignedInUserDetails,
    language: Lang
  )(implicit
    hc: HeaderCarrier
  ): Future[SubmitClaimResult] =
    claimService
      .submitClaim(
        SubmitClaimRequest(
          fillingOutClaim.draftClaim.id,
          completeClaim,
          signedInUserDetails
        ),
        language
      )
      .bimap(
        CheckYourAnswersAndSubmitController.SubmitClaimResult.SubmitClaimError,
        CheckYourAnswersAndSubmitController.SubmitClaimResult.SubmitClaimSuccess
      )
      .merge

  def submissionError(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubmitClaimFailed(request)(_ => Ok(submitClaimFailedPage()))
    }

  def confirmationOfSubmission(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withJustSubmittedClaim(request)(j => Ok(confirmationOfSubmissionPage(j)))
    }

  private def withJustSubmittedClaim(
    request: RequestWithSessionData[_]
  )(f: JustSubmittedClaim => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(j: JustSubmittedClaim) => f(j)
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withSubmitClaimFailed(
    request: RequestWithSessionData[_]
  )(
    f: Either[SubmitClaimFailed, RetrievedUserType] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubmitClaimFailed)  => f(Left(s))
      case Some(_: JustSubmittedClaim) =>
        Redirect(routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission())
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withCompleteDraftClaim(
    request: RequestWithSessionData[_]
  )(
    f: (SessionData, FillingOutClaim, CompleteClaim) => Future[Result]
  ): Future[Result] =
    request.unapply({
      case (
            sessionData,
            fillingOutClaim @ FillingOutClaim(
              _,
              signedInUserDetails,
              draftClaim: DraftC285Claim
            )
          ) =>
        CompleteC285Claim
          .fromDraftClaim(draftClaim, signedInUserDetails.verifiedEmail)
          .fold[Future[Result]](
            e => {
              logger.warn(s"could not make a complete claim", e)
              errorHandler.errorResult()(request)
            },
            s => f(sessionData, fillingOutClaim, s)
          )
    })

}

object CheckYourAnswersAndSubmitController {

  sealed trait SubmitClaimResult

  object SubmitClaimResult {

    final case class SubmitClaimError(error: Error) extends SubmitClaimResult

    final case class SubmitClaimSuccess(response: SubmitClaimResponse) extends SubmitClaimResult

  }

}
