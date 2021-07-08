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
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.data.{Form, Mapping}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, TemporaryJourneyExtractor, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.{CompleteDeclarationDetailsAnswer, IncompleteDeclarationDetailsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateDeclarationDetailsAnswer.{CompleteDuplicateDeclarationDetailsAnswer, IncompleteDuplicateDeclarationDetailAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.{Logging, TimeUtils}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  featureSwitch: FeatureSwitchService,
  enterDuplicateDeclarationDetailsPage: pages.enter_duplicate_declaration_details,
  enterDeclarationDetailsPage: pages.enter_declaration_details
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withDeclarationDetails(
    f: (
      SessionData,
      FillingOutClaim,
      DeclarationDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({
      case (
            sessionData,
            fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
          ) =>
        val maybeDeclarationAnswers = draftClaim.fold(
          _.declarationDetailsAnswer
        )
        maybeDeclarationAnswers.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteDeclarationDetailsAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
    })

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
                  .fold(
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  ) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(reference),
                          entryNumber
                        )
                      )
                    case Right(_)          =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
              case None            =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  ) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                          entryNumber
                        )
                      )
                    case Right(_)          =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber
              .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                case Left(entryNumber) =>
                  Ok(
                    enterDeclarationDetailsPage(
                      EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(ifComplete.declarationDetails),
                      entryNumber
                    )
                  )
                case Right(_)          =>
                  Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
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
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber
                      )
                    )
                  case Right(_)          =>
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
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
                  .fold(
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  ) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(reference),
                          entryNumber,
                          isAmend = true
                        )
                      )
                    case Right(_)          =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
              case None            =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  ) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                          entryNumber,
                          isAmend = true
                        )
                      )
                    case Right(_)          =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber
              .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                case Left(entryNumber) =>
                  Ok(
                    enterDeclarationDetailsPage(
                      EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(ifComplete.declarationDetails),
                      entryNumber,
                      isAmend = true
                    )
                  )
                case Right(_)          =>
                  Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
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
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber,
                        isAmend = true
                      )
                    )
                  case Right(_)          =>
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
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
                  .fold(
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  ) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDuplicateDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm.fill(reference),
                          entryNumber
                        )
                      )
                    case Right(_)          =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
              case None            =>
                fillingOutClaim.draftClaim.movementReferenceNumber
                  .fold(
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  ) {
                    case Left(entryNumber) =>
                      Ok(
                        enterDuplicateDeclarationDetailsPage(
                          EnterDeclarationDetailsController.entryDeclarationDetailsForm,
                          entryNumber
                        )
                      )
                    case Right(_)          =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber
              .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
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
                  Redirect(
                    routes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn()
                  )
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
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDuplicateDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber
                      )
                    )
                  case Right(_)          =>
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
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
                _ =>
                  Redirect(
                    routes.EnterCommoditiesDetailsController
                      .enterCommoditiesDetails(TemporaryJourneyExtractor.extractJourney)
                  )
              )
            }
          )
      }
    }
}

object EnterDeclarationDetailsController {

  final case class EntryDeclarationDetails(
    dateOfImport: DateOfImport,
    placeOfImport: String,
    importerName: String,
    importerEmailAddress: Email,
    importerPhoneNumber: PhoneNumber,
    declarantName: String,
    declarantEmailAddress: Email,
    declarantPhoneNumber: PhoneNumber
  )

  object EntryDeclarationDetails {
    implicit val format: OFormat[EntryDeclarationDetails] = Json.format[EntryDeclarationDetails]
  }

  val entryDeclarationDetailsForm: Form[EntryDeclarationDetails] = Form(
    mapping(
      "enter-declaration-details.date-of-import"          -> dateOfImportMapping(LocalDate.now),
      "enter-declaration-details.place-of-import"         -> nonEmptyText(maxLength = 70),
      "enter-declaration-details.importer-name"           -> nonEmptyText(maxLength = 70),
      "enter-declaration-details.importer-email-address"  -> Email.mappingMaxLength,
      "enter-declaration-details.importer-phone-number"   -> PhoneNumber.mapping,
      "enter-declaration-details.declarant-name"          -> nonEmptyText(maxLength = 70),
      "enter-declaration-details.declarant-email-address" -> Email.mappingMaxLength,
      "enter-declaration-details.declarant-phone-number"  -> PhoneNumber.mapping
    )(EntryDeclarationDetails.apply)(EntryDeclarationDetails.unapply)
  )

  def dateOfImportMapping(today: LocalDate): Mapping[DateOfImport] =
    mapping(
      "" -> of(
        TimeUtils.dateFormatter(
          Some(today),
          None,
          "enter-declaration-details.day",
          "enter-declaration-details.month",
          "enter-declaration-details.year",
          "enter-declaration-details"
        )
      )
    )(DateOfImport(_))(d => Some(d.value))

}
