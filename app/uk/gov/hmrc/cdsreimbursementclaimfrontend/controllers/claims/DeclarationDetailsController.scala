package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.{CompleteDeclarationDetailsAnswer, IncompleteDeclarationDetailsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateDeclarationDetailsAnswer.{CompleteDuplicateDeclarationDetailsAnswer, IncompleteDuplicateDeclarationDetailAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarationDetailsAnswer, DraftClaim, DuplicateDeclarationDetailsAnswer, Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

//TODO: Change JourneyStatus to a specific type that represents the claim type e.g. DraftC285SingleClaim
trait DeclarationDetailsController[A <: JourneyStatus] { //DraftC285Single, DraftC285Bulk
  this: FrontendController with Logging with WithAuthAndSessionDataAction with SessionUpdates =>

  val featureSwitch: FeatureSwitchService
  val sessionStore: SessionCache
  val enterDuplicateDeclarationDetailsPage: pages.enter_duplicate_declaration_details
  val enterDeclarationDetailsPage: pages.enter_declaration_details

  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext
  implicit val cc: MessagesControllerComponents
  implicit val errorHandler: ErrorHandler

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Future[Result], (SessionData, A)]

  protected val key: String
  protected val enterDeclarationDetailsCall: Call
  protected val enterDeclarationDetailsSubmitCall: Call
  protected val changeDeclarationDetailsCall: Call
  protected val changeDeclarationDetailsSubmitCall: Call

  protected def withValidJourney(request: RequestWithSessionData[_])(
    f: (SessionData, A) => Future[Result]
  ): Future[Result] =
    validJourney(request).map[Future[Result]](f.tupled).merge

  private def withDeclarationDetails(
    f: (
      SessionData,
      FillingOutClaim,
      DeclarationDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeDeclarationAnswers = draftClaim.fold(
          _.declarationDetailsAnswer
        )
        maybeDeclarationAnswers.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteDeclarationDetailsAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  private def withDuplicateDeclarationDetails(
    f: (
      SessionData,
      FillingOutClaim,
      DuplicateDeclarationDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeDuplicateDeclarantDetailAnswers = draftClaim.fold(
          _.duplicateDeclarationDetailsAnswer
        )
        maybeDuplicateDeclarantDetailAnswers.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteDuplicateDeclarationDetailAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterDeclarationDetails(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withDeclarationDetails { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.declarationDetails match {
              case Some(reference) =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(reference),
                          entryNumber,
                          key,
                          enterDeclarationDetailsSubmitCall
                        )
                      )
                    case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
              case None            =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                          entryNumber,
                          key,
                          enterDeclarationDetailsSubmitCall
                        )
                      )
                    case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber
              .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                case Left(entryNumber) =>
                  Ok(
                    enterDeclarationDetailsPage(
                      EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(ifComplete.declarationDetails),
                      entryNumber,
                      key,
                      enterDeclarationDetailsSubmitCall
                    )
                  )
                case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
              }
        )
      }
    }

  def enterDeclarationDetailsSubmit(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withDeclarationDetails { (_, fillingOutClaim, answers) =>
        EnterDeclarationDetailsController.entryDeclarationDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber
                      )
                    )
                  case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                },
            declarantDetailAnswers => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteDeclarationDetailsAnswer(
                    declarantDetailAnswers
                  ),
                complete => complete.copy(declarationDetails = declarantDetailAnswers)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(declarationDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture declaration details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
              )
            }
          )
      }
    }

  def changeDeclarationDetails(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withDeclarationDetails { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.declarationDetails match {
              case Some(reference) =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(reference),
                          entryNumber,
                          key,
                          changeDeclarationDetailsSubmitCall
                        )
                      )
                    case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
              case None            =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                          entryNumber,
                          key,
                          changeDeclarationDetailsSubmitCall
                        )
                      )
                    case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber
              .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                case Left(entryNumber) =>
                  Ok(
                    enterDeclarationDetailsPage(
                      EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(ifComplete.declarationDetails),
                      entryNumber,
                      key,
                      changeDeclarationDetailsSubmitCall
                    )
                  )
                case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
              }
        )
      }
    }

  def changeDeclarationDetailsSubmit(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withDeclarationDetails { (_, fillingOutClaim, answers) =>
        EnterDeclarationDetailsController.entryDeclarationDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber,
                        isAmend = true
                      )
                    )
                  case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                },
            declarantDetailAnswers => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteDeclarationDetailsAnswer(
                    declarantDetailAnswers
                  ),
                complete => complete.copy(declarationDetails = declarantDetailAnswers)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(declarationDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture declaration details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
              )
            }
          )
      }
    }

  def enterDuplicateDeclarationDetails(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withDuplicateDeclarationDetails { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.duplicateDeclaration match {
              case Some(reference) =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDuplicateDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(reference),
                          entryNumber
                        )
                      )
                    case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
              case None            =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDuplicateDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                          entryNumber
                        )
                      )
                    case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber
              .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                case Left(entryNumber) =>
                  ifComplete.duplicateDeclaration.fold(
                    Ok(
                      enterDuplicateDeclarationDetailsPage(
                        EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                        entryNumber
                      )
                    )
                  )(entryDeclarationDetails =>
                    Ok(
                      enterDuplicateDeclarationDetailsPage(
                        EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(entryDeclarationDetails),
                        entryNumber
                      )
                    )
                  )
                case Right(_)          =>
                  Redirect(routes.EnterMovementReferenceNumberController.enterDuplicateMrn()) //FIXME double check this
              }
        )
      }
    }

  def enterDuplicateDeclarationDetailsSubmit(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withDuplicateDeclarationDetails { (_, fillingOutClaim, answers) =>
        EnterDeclarationDetailsController.entryDeclarationDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterMrn())) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDuplicateDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber
                      )
                    )
                  case Right(_)          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                },
            declarantDetailAnswers => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteDuplicateDeclarationDetailsAnswer(
                    Some(declarantDetailAnswers)
                  ),
                complete => complete.copy(duplicateDeclaration = Some(declarantDetailAnswers))
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(duplicateDeclarationDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture duplicate declaration details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.EnterCommoditiesDetailsController.enterCommoditiesDetails())
              )
            }
          )
      }
    }

}
