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
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.{CompleteDeclarantTypeAnswer, IncompleteDeclarantTypeAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SelectWhoIsMakingTheClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  selectWhoIsMakingTheClaimPage: pages.select_who_is_making_the_claim
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withDeclarantTypeAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      DeclarantTypeAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeDeclarantType = draftClaim.fold(
          _.declarantTypeAnswer
        )
        maybeDeclarantType.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteDeclarantTypeAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def selectDeclarantType(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDeclarantTypeAnswer { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.declarantType match {
              case Some(declarantType) =>
                fillingOutClaim.draftClaim.movementReferenceNumber match {
                  case Some(movementReferenceNumber) =>
                    movementReferenceNumber.fold(
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                              declarantType
                            ),
                            routes.EnterDeclarationDetailsController.enterDeclarationDetails()
                          )
                        ),
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                              declarantType
                            ),
                            routes.CheckDeclarationDetailsController.checkDetails()
                          )
                        )
                    )
                  case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                }
              case None                =>
                fillingOutClaim.draftClaim.movementReferenceNumber match {
                  case Some(movementReferenceNumber) =>
                    movementReferenceNumber.fold(
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm,
                            routes.EnterDeclarationDetailsController.enterDeclarationDetails()
                          )
                        ),
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm,
                            routes.CheckDeclarationDetailsController.checkDetails()
                          )
                        )
                    )
                  case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber match {
              case Some(movementReferenceNumber) =>
                movementReferenceNumber.fold(
                  _ =>
                    Ok(
                      selectWhoIsMakingTheClaimPage(
                        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                          ifComplete.declarantType
                        ),
                        routes.EnterDeclarationDetailsController.enterDeclarationDetails()
                      )
                    ),
                  _ =>
                    Ok(
                      selectWhoIsMakingTheClaimPage(
                        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                          ifComplete.declarantType
                        ),
                        routes.CheckDeclarationDetailsController.checkDetails()
                      )
                    )
                )
              case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
            }
        )
      }
    }

  def selectDeclarantTypeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDeclarantTypeAnswer { (_, fillingOutClaim, answers) =>
        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber match {
                case Some(movementReferenceNumber) =>
                  movementReferenceNumber.fold(
                    _ =>
                      BadRequest(
                        selectWhoIsMakingTheClaimPage(
                          requestFormWithErrors,
                          routes.EnterDeclarationDetailsController.enterDeclarationDetails()
                        )
                      ),
                    _ =>
                      BadRequest(
                        selectWhoIsMakingTheClaimPage(
                          requestFormWithErrors,
                          routes.CheckDeclarationDetailsController.checkDetails()
                        )
                      )
                  )
                case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
              },
            declarantTypeAnswer => {
              val updatedAnswers = answers.fold(
                _ => CompleteDeclarantTypeAnswer(declarantTypeAnswer),
                complete => complete.copy(declarantType = declarantTypeAnswer)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(declarantTypeAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture declarant type", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
              )
            }
          )
      }
    }

  def changeDeclarantType(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDeclarantTypeAnswer { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.declarantType match {
              case Some(declarantType) =>
                fillingOutClaim.draftClaim.movementReferenceNumber match {
                  case Some(movementReferenceNumber) =>
                    movementReferenceNumber.fold(
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                              declarantType
                            ),
                            routes.EnterDeclarationDetailsController.enterDeclarationDetails(),
                            isAmend = true
                          )
                        ),
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                              declarantType
                            ),
                            routes.CheckDeclarationDetailsController.checkDetails(),
                            isAmend = true
                          )
                        )
                    )
                  case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                }
              case None                =>
                fillingOutClaim.draftClaim.movementReferenceNumber match {
                  case Some(movementReferenceNumber) =>
                    movementReferenceNumber.fold(
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm,
                            routes.EnterDeclarationDetailsController.enterDeclarationDetails(),
                            isAmend = true
                          )
                        ),
                      _ =>
                        Ok(
                          selectWhoIsMakingTheClaimPage(
                            SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm,
                            routes.CheckDeclarationDetailsController.checkDetails(),
                            isAmend = true
                          )
                        )
                    )
                  case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                }
            },
          ifComplete =>
            fillingOutClaim.draftClaim.movementReferenceNumber match {
              case Some(movementReferenceNumber) =>
                movementReferenceNumber.fold(
                  _ =>
                    Ok(
                      selectWhoIsMakingTheClaimPage(
                        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                          ifComplete.declarantType
                        ),
                        routes.EnterDeclarationDetailsController.enterDeclarationDetails(),
                        isAmend = true
                      )
                    ),
                  _ =>
                    Ok(
                      selectWhoIsMakingTheClaimPage(
                        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm.fill(
                          ifComplete.declarantType
                        ),
                        routes.CheckDeclarationDetailsController.checkDetails(),
                        isAmend = true
                      )
                    )
                )
              case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
            }
        )
      }
    }

  def changeDeclarantTypeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDeclarantTypeAnswer { (_, fillingOutClaim, answers) =>
        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber match {
                case Some(movementReferenceNumber) =>
                  movementReferenceNumber.fold(
                    _ =>
                      BadRequest(
                        selectWhoIsMakingTheClaimPage(
                          requestFormWithErrors,
                          routes.EnterDeclarationDetailsController.enterDeclarationDetails(),
                          isAmend = true
                        )
                      ),
                    _ =>
                      BadRequest(
                        selectWhoIsMakingTheClaimPage(
                          requestFormWithErrors,
                          routes.CheckDeclarationDetailsController.checkDetails(),
                          isAmend = true
                        )
                      )
                  )
                case None                          => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
              },
            declarantTypeAnswer => {
              val updatedAnswers = answers.fold(
                _ => CompleteDeclarantTypeAnswer(declarantTypeAnswer),
                complete => complete.copy(declarantType = declarantTypeAnswer)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(
                  _.copy(
                    declarantTypeAnswer = Some(updatedAnswers),
                    detailsRegisteredWithCdsAnswer = None,
                    contactDetailsAnswer = None,
                    reasonForBasisAndClaimAnswer = None,
                    basisOfClaimAnswer = None,
                    duplicateDeclarationDetailsAnswer = None,
                    duplicateMovementReferenceNumberAnswer = None
                  )
                )

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture declarant type", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
              )
            }
          )
      }
    }

}

object SelectWhoIsMakingTheClaimController {

  sealed trait DeclarantType extends Product with Serializable {
    def repr: String
  }

  object DeclarantType {
    final case object Importer extends DeclarantType {
      override def repr = "Importer"
    }
    final case object AssociatedWithImporterCompany extends DeclarantType {
      override def repr = "Associated with Importer Company"
    }
    final case object AssociatedWithRepresentativeCompany extends DeclarantType {
      override def repr = "Associated Representative Company"
    }

    implicit val format: OFormat[DeclarantType] = derived.oformat[DeclarantType]()
  }

  val chooseDeclarantTypeForm: Form[DeclarantType] =
    Form(
      mapping(
        "select-who-is-making-the-claim" -> number
          .verifying("invalid", a => a === 0 || a === 1 || a === 2)
          .transform[DeclarantType](
            value =>
              if (value === 0) DeclarantType.Importer
              else if (value === 1) DeclarantType.AssociatedWithImporterCompany
              else DeclarantType.AssociatedWithRepresentativeCompany,
            {
              case DeclarantType.Importer                            => 0
              case DeclarantType.AssociatedWithImporterCompany       => 1
              case DeclarantType.AssociatedWithRepresentativeCompany => 2
            }
          )
      )(identity)(Some(_))
    )
}
