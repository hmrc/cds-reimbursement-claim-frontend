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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.data.EitherT
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.i18n.Lang
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.C285ClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.C285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResult
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResult.SubmitClaimError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResult.SubmitClaimSuccess
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable

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
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  val journey: JourneyBindable = JourneyBindable.Scheduled

  val checkAllAnswers: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        implicit val router: ReimbursementRoutes = extractRoutes(fillingOutClaim.draftClaim, journey)
        implicit val subKey: Option[String]      = router.subKey
        Ok(
          checkYourAnswersPage(
            fillingOutClaim.draftClaim,
            fillingOutClaim.signedInUserDetails.verifiedEmail,
            fillingOutClaim.signedInUserDetails.eori,
            journey,
            routes.CheckYourAnswersAndSubmitController.checkAllAnswersSubmit
          )
        )
      }
    }

  val checkAllAnswersSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCompleteDraftClaim { (fillingOutClaim, completeClaim) =>
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
                routes.CheckYourAnswersAndSubmitController.submissionError
              )

            case SubmitClaimSuccess(_) =>
              logger.info(
                s"Successfully submitted claim with claim id :${completeClaim.id}"
              )
              Redirect(routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission)
          }
        )
      }

    }

  private def submitClaim(
    claim: C285Claim,
    fillingOutClaim: FillingOutClaim,
    signedInUserDetails: SignedInUserDetails,
    language: Lang
  )(implicit
    hc: HeaderCarrier
  ): Future[SubmitClaimResult] =
    claimService
      .submitClaim(
        C285ClaimRequest(
          fillingOutClaim.draftClaim.id,
          claim,
          signedInUserDetails
        ),
        language
      )
      .bimap(
        SubmitClaimError(_),
        SubmitClaimSuccess(_)
      )
      .merge

  val submissionError: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubmitClaimFailed(_ => Ok(submitClaimFailedPage()))
    }

  val confirmationOfSubmission: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withJustSubmittedClaim(claim =>
        Ok(
          confirmationOfSubmissionPage(claim.claim.totalReimbursementAmount, claim.submissionResponse.caseNumber, None)
        )
      )
    }

  private def withJustSubmittedClaim(
    f: JustSubmittedClaim => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(j: JustSubmittedClaim) => f(j)
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withSubmitClaimFailed(
    f: Either[SubmitClaimFailed, RetrievedUserType] => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubmitClaimFailed)  => f(Left(s))
      case Some(_: JustSubmittedClaim) =>
        Redirect(routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission)
      case _                           => Redirect(baseRoutes.StartController.start())
    }

  private def withCompleteDraftClaim(
    f: (FillingOutClaim, C285Claim) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.using({ case fillingOutClaim @ FillingOutClaim(_, signedInUserDetails, draftClaim: DraftClaim) =>
      C285Claim
        .fromDraftClaim(draftClaim, signedInUserDetails.verifiedEmail, signedInUserDetails.eori)
        .fold[Future[Result]](
          error => logAndDisplayError("could not make a complete claim") apply error,
          c285Claim => f(fillingOutClaim, c285Claim)
        )
    })
}
