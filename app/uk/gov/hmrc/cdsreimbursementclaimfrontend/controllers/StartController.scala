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

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType.NonGovernmentGatewayRetrievedUser
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{ContactName, Email, Name}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{Eori, GGCredId}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartController @Inject() (
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val config: Configuration,
  weOnlySupportGGPage: views.html.we_only_support_gg,
  timedOutPage: views.html.timed_out
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthRetrievalsAndSessionDataAction
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def start(): Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      (
        request.authenticatedRequest.journeyUserType,
        request.sessionData.journeyStatus
      ) match {
        case (_, Some(journeyStatus)) =>
          handleSessionJourneyStatus(journeyStatus)
        case (retrievedUserType, _)   =>
          handleRetrievedUserType(retrievedUserType)
      }
    }

  def startNewClaim(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withJustSubmittedClaim { justSubmittedClaim =>
      val result = for {
        _ <- EitherT(
               updateSession(sessionStore, request)(
                 _.copy(
                   userType = request.sessionData.flatMap(_.userType),
                   journeyStatus = Some(
                     FillingOutClaim(
                       justSubmittedClaim.ggCredId,
                       justSubmittedClaim.signedInUserDetails,
                       DraftClaim.blank
                     )
                   )
                 )
               )
             )
      } yield ()

      result.fold(
        logAndDisplayError("could not initiate claim journey:"),
        _ =>
          Redirect(
            controllers.claims.routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single)
          )
      )
    }
  }

  private def withJustSubmittedClaim(
    f: JustSubmittedClaim => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.using({ case justSubmittedClaim: JustSubmittedClaim =>
      f(justSubmittedClaim)
    })

  def weOnlySupportGG(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) => Ok(weOnlySupportGGPage())
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  def signOutAndRegisterForGG(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) =>
          Redirect(viewConfig.ggCreateAccountUrl).withNewSession
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  def signOutAndSignIn(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(NonGovernmentGatewayJourney) =>
          Redirect(routes.StartController.start()).withNewSession
        case _                                 => Redirect(routes.StartController.start())
      }
    }

  def keepAlive(): Action[AnyContent] =
    authenticatedActionWithSessionData(_ => Ok(""))

  def timedOut(): Action[AnyContent] =
    Action(implicit request => Ok(timedOutPage()))

  private def handleSessionJourneyStatus(
    journeyStatus: JourneyStatus
  ): Future[Result] =
    journeyStatus match {

      case NonGovernmentGatewayJourney =>
        Redirect(routes.StartController.weOnlySupportGG())

      case claim: FillingOutClaim =>
        Redirect(
          controllers.claims.routes.CheckYourAnswersAndSubmitController.checkAllAnswers(
            JourneyExtractor.extractJourney(claim)
          )
        )

      case claim: JustSubmittedClaim =>
        Redirect(
          controllers.claims.routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission(claim.journey)
        )

      case claim: SubmitClaimFailed =>
        Redirect(
          controllers.claims.routes.CheckYourAnswersAndSubmitController.submissionError(claim.journey)
        )
    }

  private def handleRetrievedUserType(
    retrievedUserType: RetrievedUserType
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[AnyContent]
  ): Future[Result] =
    retrievedUserType match {
      case RetrievedUserType.Individual(ggCredId, email, eori, name) =>
        handleSignedInUser(ggCredId, eori, email, name)

      case RetrievedUserType.Organisation(ggCredId, eori, name) =>
        handleSignedInUser(ggCredId, eori, None, name)

      case u: RetrievedUserType.NonGovernmentGatewayRetrievedUser =>
        handleNonGovernmentGatewayUser(u)
    }

  private def handleNonGovernmentGatewayUser(
    nonGovernmentGatewayUser: NonGovernmentGatewayRetrievedUser
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    logger.warn(
      s"User logged in with unsupported provider: ${nonGovernmentGatewayUser.authProvider}"
    )

    updateSession(sessionStore, request)(
      _.copy(
        userType = request.authenticatedRequest.userType,
        journeyStatus = Some(NonGovernmentGatewayJourney)
      )
    ).map {
      case Left(e) =>
        logAndDisplayError("could not update session:").apply(e)

      case Right(_) =>
        Redirect(routes.StartController.weOnlySupportGG())
    }

  }

  private def handleSignedInUser(
    ggCredId: GGCredId,
    eori: Eori,
    email: Option[Email],
    name: Option[Name]
  )(implicit
    request: RequestWithSessionDataAndRetrievedData[_]
  ): Future[Result] = {
    val result = for {
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(
                 userType = request.authenticatedRequest.userType,
                 journeyStatus = Some(
                   FillingOutClaim(
                     ggCredId,
                     SignedInUserDetails(
                       email,
                       eori,
                       Email(""),
                       ContactName(name.flatMap(_.name).getOrElse("No name"))
                     ),
                     DraftClaim.blank
                   )
                 )
               )
             )
           )
    } yield ()

    result.fold(
      logAndDisplayError("could not initiate claim journey:"),
      _ =>
        Redirect(
          controllers.claims.routes.CheckEoriDetailsController.show()
        )
    )
  }

}
