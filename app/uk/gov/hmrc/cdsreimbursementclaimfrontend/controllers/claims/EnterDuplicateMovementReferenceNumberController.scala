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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.enterMovementReferenceNumberKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateMovementReferenceNumberAnswer.{CompleteDuplicateMovementReferenceNumberAnswer, IncompleteDuplicateMovementReferenceNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{MrnImporter, ThirdPartyImporter}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDuplicateMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  featureSwitch: FeatureSwitchService,
  enterDuplicateMovementReferenceNumberPage: pages.enter_duplicate_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  def enterDuplicateMrn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDuplicateMovementReferenceNumberAnswer { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.duplicateMovementReferenceNumber match {
              case Some(duplicateMovementReferenceNumber) =>
                Ok(
                  enterDuplicateMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController
                      .movementReferenceNumberForm(
                        enterMovementReferenceNumberKey,
                        featureSwitch.EntryNumber.isEnabled()
                      )
                      .fill(
                        MovementReferenceNumber(duplicateMovementReferenceNumber)
                      )
                  )
                )
              case None                                   =>
                Ok(
                  enterDuplicateMovementReferenceNumberPage(
                    EnterMovementReferenceNumberController.movementReferenceNumberForm(
                      enterMovementReferenceNumberKey,
                      featureSwitch.EntryNumber.isEnabled()
                    )
                  )
                )
            },
          ifComplete =>
            ifComplete.duplicateMovementReferenceNumber.fold(
              errorHandler.errorResult()
            ) { duplicateMovementReferenceNumber =>
              Ok(
                enterDuplicateMovementReferenceNumberPage(
                  EnterMovementReferenceNumberController
                    .movementReferenceNumberForm(
                      enterMovementReferenceNumberKey,
                      featureSwitch.EntryNumber.isEnabled()
                    )
                    .fill(
                      MovementReferenceNumber(duplicateMovementReferenceNumber)
                    )
                )
              )
            }
        )
      }
    }

  def enterDuplicateMrnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDuplicateMovementReferenceNumberAnswer { (_, fillingOutClaim, answers) =>
        EnterMovementReferenceNumberController
          .movementReferenceNumberForm(
            enterMovementReferenceNumberKey,
            featureSwitch.EntryNumber.isEnabled()
          )
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterDuplicateMovementReferenceNumberPage(
                  requestFormWithErrors.copy(errors =
                    Seq(
                      EnterMovementReferenceNumberController
                        .processFormErrors(enterMovementReferenceNumberKey, requestFormWithErrors.errors)
                    )
                  )
                )
              ),
            mrnOrEntryNumber => {

              def renderPageWithError(errorKey: String): Future[Result] = {
                val form = EnterMovementReferenceNumberController
                  .movementReferenceNumberForm("enter-movement-reference-number", featureSwitch.EntryNumber.isEnabled())
                  .fill(mrnOrEntryNumber)
                  .withError("enter-movement-reference-number", s"invalid.$errorKey")
                BadRequest(
                  enterDuplicateMovementReferenceNumberPage(
                    form
                  )
                )
              }

              val mrnOrEntryValue  = mrnOrEntryNumber.value.map(_.value).leftMap(_.value).merge
              val numberHasChanged = fillingOutClaim.draftClaim.movementReferenceNumber.forall {
                case Right(cachedMrn)        => cachedMrn.value =!= mrnOrEntryValue
                case Left(cachedEntryNumber) => cachedEntryNumber.value =!= mrnOrEntryValue
                case _                       => false
              }

              mrnOrEntryNumber.value match {

                case Left(entryNumber) =>
                  val cachedMrnExists: Boolean = fillingOutClaim.draftClaim.movementReferenceNumber.forall {
                    case Right(mrn) => mrn.value.isEmpty
                    case Left(_)    => true
                  }

                  cachedMrnExists match {
                    case false =>
                      renderPageWithError("mrn-not-entry-number")

                    case true =>
                      numberHasChanged match {

                        case true =>
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

                        case false =>
                          renderPageWithError("enter-different-entry-number")
                      }
                  }
                case Right(mrn)        =>
                  val cachedEntryNumberExists: Boolean = fillingOutClaim.draftClaim.movementReferenceNumber.forall {
                    case Right(_)          => true
                    case Left(entryNumber) => entryNumber.value.isEmpty
                  }

                  cachedEntryNumberExists match {

                    case false =>
                      renderPageWithError("entry-number-not-mrn")

                    case true =>
                      numberHasChanged match {

                        case true =>
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

                          val getDeclaration: EitherT[Future, Error, Option[DisplayDeclaration]] = claimService
                            .getDisplayDeclaration(mrn)
                            .leftMap(_ => Error("could not get duplicate declaration"))

                          val result: EitherT[Future, Error, Either[MrnImporter, ThirdPartyImporter]] = for {
                            maybeDisplayDeclaration <- getDeclaration
                            _                       <- updateSessionWithReference
                            mrnJourneyFlow          <-
                              EitherT
                                .fromEither[Future](
                                  EnterMovementReferenceNumberController.evaluateMrnJourneyFlow(
                                    fillingOutClaim.signedInUserDetails,
                                    maybeDisplayDeclaration
                                  )
                                )
                                .leftMap(_ => Error("could not evaluate MRN flow"))
                            displayDeclaration      <-
                              EitherT
                                .fromOption[Future](
                                  maybeDisplayDeclaration,
                                  Error("could not unbox display declaration")
                                )
                            _                       <- EitherT.liftF(
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
                              Redirect(baseRoutes.IneligibleController.ineligible())
                            },
                            {
                              case Left(_)  => Redirect(routes.CheckDeclarationDetailsController.checkDuplicateDetails())
                              case Right(_) =>
                                Redirect(routes.EnterImporterEoriNumberController.enterImporterEoriNumber())
                            }
                          )

                        case false =>
                          renderPageWithError("enter-different-mrn")
                      }
                  }
              }
            }
          )
      }
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

}
