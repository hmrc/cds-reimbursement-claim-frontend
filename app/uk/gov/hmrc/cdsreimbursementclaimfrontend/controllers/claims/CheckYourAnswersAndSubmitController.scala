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
import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Forms, Mapping}
import play.api.i18n.Lang
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult.{SubmitClaimError, SubmitClaimSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{CompleteC285Claim, CompleteClaim, Error, RetrievedUserType, SessionData, SubmitClaimRequest, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class CheckYourAnswersAndSubmitController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  claimService: ClaimService,
  checkYourAnswersPage: pages.check_your_answers,
  confirmationOfSubmissionPage: pages.confirmation_of_submission,
  submitClaimFailedPage: pages.submit_claim_error
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def checkAllAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCompleteDraftClaim(request) { (_, _, completeClaim) =>
        Ok(
          checkYourAnswersPage(
            CheckYourAnswersAndSubmitController.confirmDetailsForms,
            completeClaim,
            fileUploadRoutes.SupportingEvidenceController.checkYourAnswers()
          )
        )
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
          { e =>
            logger.warn("Error while trying to update session", e)
            errorHandler.errorResult()
          },
          {
            case SubmitClaimError(e) =>
              logger.warn(s"Could not submit return}", e)
              Redirect(
                claimsRoutes.CheckYourAnswersAndSubmitController.submissionError()
              )

            case SubmitClaimSuccess(_) =>
              logger.info(
                s"Successfully submitted claim with claim id :${completeClaim.claimId}"
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
    language: Lang
  )(implicit
    hc: HeaderCarrier
  ): Future[SubmitClaimResult] =
    claimService
      .submitClaim(
        SubmitClaimRequest(
          fillingOutClaim.draftClaim.id,
          completeClaim
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
      withSubmitClaimFailedOrSubscribed(request)(_ => Ok(submitClaimFailedPage()))
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

  private def withSubmitClaimFailedOrSubscribed(
    request: RequestWithSessionData[_]
  )(
    f: Either[SubmitClaimFailed, RetrievedUserType] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubmitClaimFailed) => f(Left(s))
      // case Some(s: SignedInUser)      => f(Right(s))
      case _                          => Redirect(baseRoutes.StartController.start())
    }

  private def withCompleteDraftClaim(
    request: RequestWithSessionData[_]
  )(
    f: (SessionData, FillingOutClaim, CompleteClaim) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              s,
              r @ FillingOutClaim(
                _,
                _,
                draftClaim: DraftC285Claim
              )
            )
          ) =>
        CompleteC285Claim
          .fromDraftClaim(draftClaim)
          .fold[Future[Result]] {
            logger.warn(s"could not make a complete claim ${draftClaim.toString}")
            Redirect(claimsRoutes.EnterMovementReferenceNumberController.enterMrn())
          }(f(s, r, _))
      case _ =>
        Redirect(baseRoutes.StartController.start())
    }

}

object CheckYourAnswersAndSubmitController {

  sealed trait SubmitClaimResult

  object SubmitClaimResult {

    final case class SubmitClaimError(error: Error) extends SubmitClaimResult

    final case class SubmitClaimSuccess(response: SubmitClaimResponse) extends SubmitClaimResult

  }

  def readValue[T](
    key: String,
    data: Map[String, String],
    f: String => T,
    requiredErrorArgs: Seq[String] = Seq.empty
  ): Either[FormError, T] =
    data
      .get(key)
      .map(_.trim())
      .filter(_.nonEmpty)
      .fold[Either[FormError, T]](Left(FormError(key, "error.required", requiredErrorArgs))) { stringValue =>
        Either
          .fromTry(Try(f(stringValue)))
          .leftMap(_ => FormError(key, "error.invalid"))
      }

  val confirmDetails: Mapping[List[Int]] = {
    val confirmFormatter: Formatter[Int] =
      new Formatter[Int] {

        override def bind(
          key: String,
          data: Map[String, String]
        ): Either[Seq[FormError], Int] =
          readValue(key, data, identity)
            .flatMap {
              case "0" => Right(0)
              case _   => Left(FormError(key, "error.invalid"))
            }
            .leftMap(Seq(_))

        override def unbind(
          key: String,
          value: Int
        ): Map[String, String] =
          Map(key -> value.toString)
      }

    mapping(
      "check-your-answers.declaration" -> Forms
        .list(of(confirmFormatter))
        .verifying("error.required", _.nonEmpty)
    )(identity)(Some(_))

  }

  val confirmDetailsForms: Form[Confirmation] = Form(
    mapping(
      "" -> confirmDetails
    )(Confirmation.apply)(Confirmation.unapply)
  )

  final case class Confirmation(
    hasConfirmed: List[Int]
  )

  object Confirmation {
    implicit val format: OFormat[Confirmation] = Json.format[Confirmation]
  }

}
