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
import cats.implicits.{catsSyntaxEq, _}
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.libs.json.OFormat
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.{CheckDeclarationDetailsAnswer, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, TemporaryJourneyExtractor}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[CheckDeclarationDetailsAnswer] = _.checkDeclarationDetailsAnswer

  private def withPossibleDeclaration(
    f: (
      SessionData,
      FillingOutClaim,
      Option[DisplayDeclaration]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (s, r @ FillingOutClaim(_, _, c: DraftClaim)) =>
      val maybeDisplayDeclaration = c.fold(_.displayDeclaration)
      f(s, r, maybeDisplayDeclaration)
    })

  private def withDuplicateDeclaration(
    f: (
      SessionData,
      FillingOutClaim,
      Option[DisplayDeclaration]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (s, r @ FillingOutClaim(_, _, c: DraftClaim)) =>
      val maybeDisplayDeclaration = c.fold(_.duplicateDisplayDeclaration)
      f(s, r, maybeDisplayDeclaration)
    })

  def checkDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withPossibleDeclaration { (_, _, maybeDeclaration) =>
      maybeDeclaration.fold(
        Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
      )(declaration =>
        Ok(
          checkDeclarationDetailsPage(
            declaration,
            routes.CheckDeclarationDetailsController.checkDetailsSubmit(),
            checkDeclarationDetailsAnswerForm
          )
        )
      )
    }
  }

  def checkDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[CheckDeclarationDetailsAnswer] { (fillingOutClaim, _) =>
      CheckDeclarationDetailsController.checkDeclarationDetailsAnswerForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            fillingOutClaim.draftClaim
              .fold(_.displayDeclaration)
              .map(declaration =>
                Future.successful(
                  BadRequest(
                    checkDeclarationDetailsPage(
                      declaration,
                      routes.CheckDeclarationDetailsController.checkDetailsSubmit(),
                      formWithErrors
                    )
                  )
                )
              )
              .getOrElse(Future.successful(errorHandler.errorResult())),
          { answer =>
            val newDraftClaim  = fillingOutClaim.draftClaim.fold(_.copy(checkDeclarationDetailsAnswer = Some(answer)))
            val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

            val result = EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
              .leftMap(_ => Error("could not update session"))

            result.fold(
              e => {
                logger.warn("could not get radio button details", e)
                errorHandler.errorResult()
              },
              _ =>
                answer match {
                  case DeclarationAnswersAreCorrect =>
                    //single journey
                    Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                  //schedule journey

                  case DeclarationAnswersAreIncorrect =>
                    //single journey

                    //schedule journey
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Scheduled))
                }
            )
          }
        )

    }
  }

  def checkDuplicateDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withDuplicateDeclaration { (_, _, maybeDeclaration) =>
      maybeDeclaration.fold(
        Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
      )(declaration =>
        Ok(
          checkDeclarationDetailsPage(
            declaration,
            routes.CheckDeclarationDetailsController.checkDuplicateDetailsSubmit(),
            checkDeclarationDetailsAnswerForm
          )
        )
      )
    }
  }

  def checkDuplicateDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Redirect(routes.EnterCommoditiesDetailsController.enterCommoditiesDetails(TemporaryJourneyExtractor.extractJourney))
  }

}

object CheckDeclarationDetailsController {

  sealed trait CheckDeclarationDetailsAnswer extends Product with Serializable

  case object DeclarationAnswersAreCorrect extends CheckDeclarationDetailsAnswer
  case object DeclarationAnswersAreIncorrect extends CheckDeclarationDetailsAnswer

  implicit val checkDeclarationDetailsAnswerFormat: OFormat[CheckDeclarationDetailsAnswer] =
    derived.oformat[CheckDeclarationDetailsAnswer]()

  val messageKey = "check-declaration-details"

  val checkDeclarationDetailsAnswerForm: Form[CheckDeclarationDetailsAnswer] =
    Form(
      mapping(
        messageKey -> number
          .verifying("invalid", a => a === 0 || a === 1)
          .transform[CheckDeclarationDetailsAnswer](
            value =>
              if (value === 0) DeclarationAnswersAreCorrect
              else DeclarationAnswersAreIncorrect,
            {
              case DeclarationAnswersAreCorrect   => 0
              case DeclarationAnswersAreIncorrect => 1
            }
          )
      )(identity)(Some(_))
    )
}
