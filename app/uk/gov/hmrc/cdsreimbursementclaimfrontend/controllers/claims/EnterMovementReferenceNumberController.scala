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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateMovementReferenceNumberAnswer.{CompleteDuplicateMovementReferenceNumberAnswer, IncompleteDuplicateMovementReferenceNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.{CompleteMovementReferenceNumberAnswer, IncompleteMovementReferenceNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{MrnImporter, ThirdPartyImporter}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
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
  enterDuplicateMovementReferenceNumberPage: pages.enter_duplicate_movement_reference_number,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
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

  private def withDuplicateMovementReferenceNumberAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      DuplicateMovementReferenceNumberAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeDuplicateMovementReferenceNumberAnswer = draftClaim.fold(
          _.duplicateMovementReferenceNumberAnswer
        )
        maybeDuplicateMovementReferenceNumberAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteDuplicateMovementReferenceNumberAnswer.empty)
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
                Ok(
                  enterMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController.movementReferenceNumberForm
                  )
                )
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
                  val updatedAnswers: CompleteMovementReferenceNumberAnswer = answers.fold(
                    _ =>
                      CompleteMovementReferenceNumberAnswer(
                        Right(mrn)
                      ),
                    complete => complete.copy(movementReferenceNumber = Right(mrn))
                  )
                  val newDraftClaim                                         =
                    fillingOutClaim.draftClaim.fold(_.copy(movementReferenceNumberAnswer = Some(updatedAnswers)))

                  val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                  val updateSessionWithReference = EitherT
                    .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                    .leftMap((_: Unit) => Error("could not update session"))

                  val getDeclaration: EitherT[Future, Error, DisplayDeclaration] = claimService
                    .getDisplayDeclaration(mrn)
                    .leftMap(_ => Error("could not get declaration"))

                  val result: EitherT[Future, Error, Either[MrnImporter, ThirdPartyImporter]] = for {
                    displayDeclaration <- getDeclaration
                    _                  <- updateSessionWithReference
                    mrnJourneyFlow     <-
                      EitherT
                        .fromEither[Future](
                          evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, displayDeclaration)
                        )
                        .leftMap(_ => Error("could not evaluate MRN flow"))
                    _                  <- EitherT.liftF(
                                            updateSession(sessionStore, request)(
                                              _.copy(journeyStatus =
                                                Some(
                                                  fillingOutClaim.copy(draftClaim =
                                                    newDraftClaim.copy(
                                                      displayDeclaration = Some(displayDeclaration),
                                                      movementReferenceNumberAnswer = Some(updatedAnswers)
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                  } yield mrnJourneyFlow

                  result.fold(
                    e => {
                      logger.warn("could not get declaration information", e)
                      errorHandler.errorResult()
                    },
                    {
                      case Left(_)  => Redirect(routes.CheckDeclarantDetailsController.checkDetails())
                      case Right(_) => Redirect(routes.EnterImporterEoriNumberController.enterImporterEoriNumber())
                    }
                  )
              }
          )
      }
    }

  def changeMrn(): Action[AnyContent] =
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
                    ),
                    isAmend = true
                  )
                )
              case None                          =>
                Ok(
                  enterMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController.movementReferenceNumberForm,
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              enterMovementReferenceNumberPage(
                EnterMovementReferenceNumberController.movementReferenceNumberForm.fill(
                  MovementReferenceNumber(ifComplete.movementReferenceNumber)
                ),
                isAmend = true
              )
            )
        )
      }
    }

  def changeMrnSubmit(): Action[AnyContent] =
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
                  ),
                  isAmend = true
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
                    DraftC285Claim.newDraftC285Claim.copy(movementReferenceNumberAnswer = Some(updatedAnswers))

                  val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                  val result = EitherT
                    .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                    .leftMap((_: Unit) => Error("could not update session"))

                  result.fold(
                    e => {
                      logger.warn("could not capture change in entry number", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.EnterDeclarationDetailsController.enterDeclarationDetails())
                  )
                case Right(mrn)        =>
                  val updatedAnswers: CompleteMovementReferenceNumberAnswer = answers.fold(
                    _ =>
                      CompleteMovementReferenceNumberAnswer(
                        Right(mrn)
                      ),
                    complete => complete.copy(movementReferenceNumber = Right(mrn))
                  )
                  val newDraftClaim                                         =
                    DraftC285Claim.newDraftC285Claim.copy(movementReferenceNumberAnswer = Some(updatedAnswers))

                  val result: EitherT[Future, models.Error, Unit] = for {
                    displayDeclaration <-
                      claimService.getDisplayDeclaration(mrn).leftMap(_ => Error("could not get declaration"))
                    updatedJourney      =
                      fillingOutClaim.copy(draftClaim =
                        if (
                          displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI === fillingOutClaim.signedInUserDetails.eori.value
                        )
                          newDraftClaim.copy(
                            displayDeclaration = Some(displayDeclaration),
                            movementReferenceNumberAnswer = Some(updatedAnswers)
                          )
                        else newDraftClaim
                      )
                    _                  <-
                      EitherT
                        .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                        .leftMap((_: Unit) => Error("could not update session"))
                  } yield ()

                  result.fold(
                    e => {
                      logger.warn("could not capture change to mrn number", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.CheckDeclarantDetailsController.checkDetails())
                  )
              }
          )
      }
    }

  def enterDuplicateMrn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDuplicateMovementReferenceNumberAnswer { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.duplicateMovementReferenceNumber match {
              case Some(duplicateMovementReferenceNumber) =>
                Ok(
                  enterDuplicateMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController.movementReferenceNumberForm.fill(
                      MovementReferenceNumber(duplicateMovementReferenceNumber)
                    )
                  )
                )
              case None                                   =>
                Ok(
                  enterDuplicateMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController.movementReferenceNumberForm
                  )
                )
            },
          ifComplete =>
            ifComplete.duplicateMovementReferenceNumber.fold(
              errorHandler.errorResult()
            )(duplicateMovementReferenceNumber =>
              Ok(
                enterDuplicateMovementReferenceNumberPage(
                  EnterMovementReferenceNumberController.movementReferenceNumberForm.fill(
                    MovementReferenceNumber(duplicateMovementReferenceNumber)
                  )
                )
              )
            )
        )
      }
    }

  def enterDuplicateMrnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDuplicateMovementReferenceNumberAnswer { (_, fillingOutClaim, answers) =>
        EnterMovementReferenceNumberController.movementReferenceNumberForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterDuplicateMovementReferenceNumberPage(
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
                      CompleteDuplicateMovementReferenceNumberAnswer(
                        Some(Left(entryNumber))
                      ),
                    complete => complete.copy(duplicateMovementReferenceNumber = Some(Left(entryNumber)))
                  )
                  val newDraftClaim  =
                    fillingOutClaim.draftClaim
                      .fold(_.copy(duplicateMovementReferenceNumberAnswer = Some(updatedAnswers)))

                  val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                  val result = EitherT
                    .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                    .leftMap((_: Unit) => Error("could not update session"))

                  result.fold(
                    e => {
                      logger.warn("could not capture entry number", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.EnterDeclarationDetailsController.enterDuplicateDeclarationDetails())
                  )
                case Right(mrn)        =>
                  val updatedAnswers = answers.fold(
                    _ =>
                      CompleteDuplicateMovementReferenceNumberAnswer(
                        Some(Right(mrn))
                      ),
                    complete => complete.copy(duplicateMovementReferenceNumber = Some(Right(mrn)))
                  )
                  val newDraftClaim  =
                    fillingOutClaim.draftClaim
                      .fold(_.copy(duplicateMovementReferenceNumberAnswer = Some(updatedAnswers)))

                  val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                  val updateSessionWithReference = EitherT
                    .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                    .leftMap((_: Unit) => Error("could not update session"))

                  val getDeclaration: EitherT[Future, Error, DisplayDeclaration] = claimService
                    .getDisplayDeclaration(mrn)
                    .leftMap(_ => Error("could not get duplicate declaration"))

                  val result: EitherT[Future, Error, Either[MrnImporter, ThirdPartyImporter]] = for {
                    displayDeclaration <- getDeclaration
                    _                  <- updateSessionWithReference
                    mrnJourneyFlow     <-
                      EitherT
                        .fromEither[Future](
                          evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, displayDeclaration)
                        )
                        .leftMap(_ => Error("could not evaluate MRN flow"))
                    _                  <- EitherT.liftF(
                                            updateSession(sessionStore, request)(
                                              _.copy(journeyStatus =
                                                Some(
                                                  fillingOutClaim.copy(draftClaim =
                                                    newDraftClaim.copy(
                                                      duplicateDisplayDeclaration = Some(displayDeclaration),
                                                      duplicateMovementReferenceNumberAnswer = Some(updatedAnswers)
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                  } yield mrnJourneyFlow

                  result.fold(
                    e => {
                      logger.warn("could not get declaration information", e)
                      errorHandler.errorResult()
                    },
                    {
                      case Left(_)  => Redirect(routes.CheckDeclarantDetailsController.checkDuplicateDetails())
                      case Right(_) => Redirect(routes.EnterImporterEoriNumberController.enterImporterEoriNumber())
                    }
                  )
              }
          )
      }
    }

  private def evaluateMrnJourneyFlow(
    signedInUserDetails: SignedInUserDetails,
    displayDeclaration: DisplayDeclaration
  ): Either[Error, Either[MrnImporter, ThirdPartyImporter]] =
    (
      displayDeclaration.displayResponseDetail.consigneeDetails,
      Some(displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI)
    ) match {
      case (None, _)           => Right(Right(ThirdPartyImporter(displayDeclaration)))
      case (Some(cd), Some(d)) =>
        if (cd.consigneeEORI === signedInUserDetails.eori.value) Right(Left(MrnImporter(displayDeclaration)))
        else if (cd.consigneeEORI =!= signedInUserDetails.eori.value || d =!= signedInUserDetails.eori.value)
          Right(Right(ThirdPartyImporter(displayDeclaration)))
        else Right(Right(ThirdPartyImporter(displayDeclaration)))
      case _                   => Left(Error("could not determine if signed in user's Eori matches any on the declaration"))
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
