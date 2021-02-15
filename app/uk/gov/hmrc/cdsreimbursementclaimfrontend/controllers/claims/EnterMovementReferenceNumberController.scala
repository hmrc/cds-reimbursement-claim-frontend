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
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.MovementReferenceNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.{CompleteMovementReferenceNumberAnswer, IncompleteMovementReferenceNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error, MovementReferenceNumberAnswer, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  claimService: ClaimService,
  cc: MessagesControllerComponents,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number,
  mrnNotLinkedToEoriPage: pages.mrn_not_linked_to_eori
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withMovementReferenceNumberAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      MovementReferenceNumberAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeMovementReferenceNumberAnswers = draftClaim.fold(
          _.movementReferenceNumberAnswer
        )
        maybeMovementReferenceNumberAnswers.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteMovementReferenceNumberAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterMrn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMovementReferenceNumberAnswer { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.movementReferenceNumber match {
              case Some(movementReferenceNumber) =>
                Ok(
                  enterMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController.movementReferenceNumberForm.fill(
                      MovementReferenceNumber(movementReferenceNumber)
                    )
                  )
                )
              case None                          =>
                Ok(enterMovementReferenceNumberPage(EnterMovementReferenceNumberController.movementReferenceNumberForm))
            },
          ifComplete =>
            Ok(
              enterMovementReferenceNumberPage(
                EnterMovementReferenceNumberController.movementReferenceNumberForm.fill(
                  MovementReferenceNumber(ifComplete.movementReferenceNumber)
                )
              )
            )
        )
      }
    }

  def enterMrnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withMovementReferenceNumberAnswer { (_, fillingOutClaim, answers) =>
        EnterMovementReferenceNumberController.movementReferenceNumberForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterMovementReferenceNumberPage(
                  requestFormWithErrors.copy(errors =
                    Seq(EnterMovementReferenceNumberController.processFormErrors(requestFormWithErrors.errors))
                  )
                )
              ),
            movementReferenceNumber =>
              movementReferenceNumber.value match {
                case Left(entryNumber) =>
                  val updatedAnswers = answers.fold(
                    _ =>
                      CompleteMovementReferenceNumberAnswer(
                        Left(entryNumber)
                      ),
                    complete => complete.copy(movementReferenceNumber = Left(entryNumber))
                  )
                  val newDraftClaim  =
                    fillingOutClaim.draftClaim.fold(_.copy(movementReferenceNumberAnswer = Some(updatedAnswers)))

                  val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                  val result = EitherT
                    .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                    .leftMap((_: Unit) => Error("could not update session"))

                  result.fold(
                    e => {
                      logger.warn("could not capture entry number", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.EnterDeclarationDetailsController.enterDeclarationDetails())
                  )
                case Right(mrn)        =>
                  val updatedAnswers = answers.fold(
                    _ =>
                      CompleteMovementReferenceNumberAnswer(
                        Right(mrn)
                      ),
                    complete => complete.copy(movementReferenceNumber = Right(mrn))
                  )
                  val newDraftClaim  =
                    fillingOutClaim.draftClaim.fold(_.copy(movementReferenceNumberAnswer = Some(updatedAnswers)))

                  val result: EitherT[Future, models.Error, Unit] = for {
                    declaration   <- claimService.getDeclaration(mrn).leftMap(_ => Error("could not get declaration"))
                    updatedJourney =
                      fillingOutClaim.copy(draftClaim =
                        if (
                          declaration.declarantDetails.declarantEORI === fillingOutClaim.signedInUserDetails.eori.value
                        )
                          newDraftClaim.copy(
                            maybeDeclaration = Some(declaration),
                            movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Right(mrn)))
                          )
                        else newDraftClaim
                      )
                    _             <-
                      EitherT
                        .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                        .leftMap((_: Unit) => Error("could not update session"))
                  } yield ()

                  result.fold(
                    e => {
                      logger.warn("could not capture movement reference number", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.CheckDeclarantDetailsController.checkDetails())
                  )
              }
          )
      }
    }

  def mrnNotLinkedToEori(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(mrnNotLinkedToEoriPage(routes.EnterMovementReferenceNumberController.enterMrn()))
  }

}

object EnterMovementReferenceNumberController {

  final case class MovementReferenceNumber(value: Either[EntryNumber, MRN]) extends AnyVal

  val movementReferenceNumberMapping: Mapping[Either[EntryNumber, MRN]] =
    nonEmptyText
      .verifying("invalid.number", str => MRN.isValid(str) | EntryNumber.isValid(str))
      .transform[Either[EntryNumber, MRN]](
        str => if (MRN.isValid(str)) Right(MRN(str)) else Left(EntryNumber(str)),
        {
          case Left(entryNumber) => entryNumber.value
          case Right(mrn)        => mrn.value
        }
      )

  val movementReferenceNumberForm: Form[MovementReferenceNumber] = Form(
    mapping(
      "enter-movement-reference-number" -> movementReferenceNumberMapping
    )(MovementReferenceNumber.apply)(MovementReferenceNumber.unapply)
  )

  def processFormErrors(errors: Seq[FormError]): FormError =
    if (errors.exists(fe => fe.message === "error.required")) {
      FormError("enter-movement-reference-number", List("error.required"))
    } else if (errors.exists(fe => fe.message === "invalid.reference"))
      FormError("enter-movement-reference-number", List("invalid.reference"))
    else
      FormError("enter-movement-reference-number", List("invalid"))

}
