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
import play.api.data.Forms.{mapping, number}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectReasonForClaimController.SelectReasonForClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaimAnswer.{CompleteBasisOfClaimAnswer, IncompleteBasisOfClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SelectReasonForClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  selectReasonForClaimPage: pages.select_reason_for_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withSelectReasonForClaim(
    f: (
      SessionData,
      FillingOutClaim,
      BasisOfClaimAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              s,
              r @ FillingOutClaim(_, _, c: DraftClaim)
            )
          ) =>
        val maybeReasonForClaim = c.fold(
          _.basisOfClaimAnswer
        )
        maybeReasonForClaim.fold[Future[Result]](
          f(s, r, IncompleteBasisOfClaimAnswer.empty)
        )(f(s, r, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def selectReasonForClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.maybeBasisOfClaim match {
              case Some(reasonForClaimOption) =>
                Ok(
                  selectReasonForClaimPage(
                    SelectReasonForClaimController.reasonForClaimForm.fill(SelectReasonForClaim(reasonForClaimOption)),
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
                  )
                )
              case None                       =>
                Ok(
                  selectReasonForClaimPage(
                    SelectReasonForClaimController.reasonForClaimForm,
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
                  )
                )
            },
          ifComplete =>
            Ok(
              selectReasonForClaimPage(
                SelectReasonForClaimController.reasonForClaimForm.fill(
                  SelectReasonForClaim(ifComplete.basisOfClaim)
                ),
                routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
              )
            )
        )
      }
    }

  def selectReasonForClaimSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, fillingOutClaim, answers) =>
        SelectReasonForClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                selectReasonForClaimPage(
                  requestFormWithErrors,
                  routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
                )
              ),
            reasonForClaim => {
              val updatedAnswers: BasisOfClaimAnswer = answers.fold(
                _ => CompleteBasisOfClaimAnswer(reasonForClaim.reasonForClaimOption),
                complete => complete.copy(basisOfClaim = reasonForClaim.reasonForClaimOption)
              )
              val newDraftClaim                      =
                fillingOutClaim.draftClaim
                  .fold(_.copy(basisOfClaimAnswer = Some(updatedAnswers), reasonForBasisAndClaimAnswer = None))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not store reason for claim answer", e)
                  errorHandler.errorResult()
                },
                _ =>
                  reasonForClaim.reasonForClaimOption match {
                    case BasisOfClaim.DuplicateMrnEntry =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterDuplicateMrn())
                    case _                              => Redirect(routes.EnterCommoditiesDetailsController.enterCommoditiesDetails())
                  }
              )
            }
          )
      }

    }

  def changeReasonForClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.maybeBasisOfClaim match {
              case Some(reasonForClaimOption) =>
                Ok(
                  selectReasonForClaimPage(
                    SelectReasonForClaimController.reasonForClaimForm.fill(SelectReasonForClaim(reasonForClaimOption)),
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                    true
                  )
                )
              case None                       =>
                Ok(
                  selectReasonForClaimPage(
                    SelectReasonForClaimController.reasonForClaimForm,
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                    true
                  )
                )
            },
          ifComplete =>
            Ok(
              selectReasonForClaimPage(
                SelectReasonForClaimController.reasonForClaimForm.fill(
                  SelectReasonForClaim(ifComplete.basisOfClaim)
                ),
                routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                true
              )
            )
        )
      }
    }

  def changeReasonForClaimSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, fillingOutClaim, answers) =>
        SelectReasonForClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                selectReasonForClaimPage(
                  requestFormWithErrors,
                  routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                  true
                )
              ),
            reasonForClaim => {
              val updatedAnswers: BasisOfClaimAnswer = answers.fold(
                _ => CompleteBasisOfClaimAnswer(reasonForClaim.reasonForClaimOption),
                complete => complete.copy(basisOfClaim = reasonForClaim.reasonForClaimOption)
              )
              val newDraftClaim                      =
                fillingOutClaim.draftClaim.fold(_.copy(basisOfClaimAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not store reason for claim answer", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
              )
            }
          )
      }

    }
}

object SelectReasonForClaimController {

  final case class SelectReasonForClaim(
    reasonForClaimOption: BasisOfClaim
  )

  val reasonForClaimForm: Form[SelectReasonForClaim] =
    Form(
      mapping(
        "select-basis-for-claim" -> number
          .verifying(
            "invalid reason for claim",
            reason =>
              reason === 0 ||
                reason === 1 ||
                reason === 2 ||
                reason === 3 ||
                reason === 4 ||
                reason === 5 ||
                reason === 6 ||
                reason === 7 ||
                reason === 8 ||
                reason === 9 ||
                reason === 10 ||
                reason === 11 ||
                reason === 12 ||
                reason === 13
          )
          .transform[BasisOfClaim](
            {
              case 0  => BasisOfClaim.DuplicateMrnEntry
              case 1  => BasisOfClaim.DutySuspension
              case 2  => BasisOfClaim.EndUseRelief
              case 3  => BasisOfClaim.IncorrectCommodityCode
              case 4  => BasisOfClaim.IncorrectCpc
              case 5  => BasisOfClaim.IncorrectValue
              case 6  => BasisOfClaim.IncorrectEoriAndDefermentAccountNumber
              case 7  => BasisOfClaim.InwardProcessingReliefFromCustomsDuty
              case 8  => BasisOfClaim.OutwardProcessingRelief
              case 9  => BasisOfClaim.Preference
              case 10 => BasisOfClaim.ProofOfReturnRefundGiven
            },
            {
              case BasisOfClaim.DuplicateMrnEntry                      => 0
              case BasisOfClaim.DutySuspension                         => 1
              case BasisOfClaim.EndUseRelief                           => 2
              case BasisOfClaim.IncorrectCommodityCode                 => 3
              case BasisOfClaim.IncorrectCpc                           => 4
              case BasisOfClaim.IncorrectValue                         => 5
              case BasisOfClaim.IncorrectEoriAndDefermentAccountNumber => 6
              case BasisOfClaim.InwardProcessingReliefFromCustomsDuty  => 7
              case BasisOfClaim.OutwardProcessingRelief                => 8
              case BasisOfClaim.Preference                             => 9
              case BasisOfClaim.ProofOfReturnRefundGiven               => 10
            }
          )
      )(SelectReasonForClaim.apply)(SelectReasonForClaim.unapply)
    )

}
