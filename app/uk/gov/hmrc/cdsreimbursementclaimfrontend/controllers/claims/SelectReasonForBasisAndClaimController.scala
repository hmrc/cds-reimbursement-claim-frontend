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
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonAndBasisOfClaimAnswer.{CompleteReasonAndBasisOfClaimAnswer, IncompleteReasonAndBasisOfClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SelectReasonForBasisAndClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  selectReasonForClaimAndBasisPage: pages.select_reason_and_basis_for_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withSelectReasonForClaim(
    f: (
      SessionData,
      FillingOutClaim,
      ReasonAndBasisOfClaimAnswer
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
          _.reasonForBasisAndClaimAnswer
        )
        maybeReasonForClaim.fold[Future[Result]](
          f(s, r, IncompleteReasonAndBasisOfClaimAnswer.empty)
        )(f(s, r, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def selectReasonForClaimAndBasis(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.maybeSelectReasonForClaimAndBasis match {
              case Some(selectReasonForClaimAndBasis) =>
                Ok(
                  selectReasonForClaimAndBasisPage(
                    SelectReasonForBasisAndClaimController.reasonForClaimForm.fill(
                      selectReasonForClaimAndBasis
                    ),
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
                  )
                )
              case None                               =>
                Ok(
                  selectReasonForClaimAndBasisPage(
                    SelectReasonForBasisAndClaimController.reasonForClaimForm,
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
                  )
                )
            },
          ifComplete =>
            Ok(
              selectReasonForClaimAndBasisPage(
                SelectReasonForBasisAndClaimController.reasonForClaimForm.fill(
                  ifComplete.selectReasonForBasisAndClaim
                ),
                routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
              )
            )
        )
      }
    }

  def selectReasonForClaimAndBasisSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, fillingOutClaim, answers) =>
        SelectReasonForBasisAndClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                selectReasonForClaimAndBasisPage(
                  requestFormWithErrors,
                  routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual()
                )
              ),
            reasonForClaimAndBasis => {
              val updatedAnswers: ReasonAndBasisOfClaimAnswer = answers.fold(
                _ => CompleteReasonAndBasisOfClaimAnswer(reasonForClaimAndBasis),
                complete => complete.copy(selectReasonForBasisAndClaim = reasonForClaimAndBasis)
              )
              val newDraftClaim                               =
                fillingOutClaim.draftClaim
                  .fold(_.copy(reasonForBasisAndClaimAnswer = Some(updatedAnswers), basisOfClaimAnswer = None))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not store reason for reason and basis answer", e)
                  errorHandler.errorResult()
                },
                _ =>
                  reasonForClaimAndBasis.basisForClaim match {
                    case BasisOfClaim.DuplicateEntry =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterDuplicateMrn())
                    case _                           => Redirect(routes.EnterCommoditiesDetailsController.enterCommoditiesDetails())
                  }
              )
            }
          )
      }

    }

  def changeReasonForClaimAndBasis(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.maybeSelectReasonForClaimAndBasis match {
              case Some(selectReasonForClaimAndBasis) =>
                Ok(
                  selectReasonForClaimAndBasisPage(
                    SelectReasonForBasisAndClaimController.reasonForClaimForm.fill(
                      selectReasonForClaimAndBasis
                    ),
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                    isAmend = true
                  )
                )
              case None                               =>
                Ok(
                  selectReasonForClaimAndBasisPage(
                    SelectReasonForBasisAndClaimController.reasonForClaimForm,
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              selectReasonForClaimAndBasisPage(
                SelectReasonForBasisAndClaimController.reasonForClaimForm.fill(
                  ifComplete.selectReasonForBasisAndClaim
                ),
                routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                isAmend = true
              )
            )
        )
      }
    }

  def changeReasonForClaimAndBasisSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, fillingOutClaim, answers) =>
        SelectReasonForBasisAndClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                selectReasonForClaimAndBasisPage(
                  requestFormWithErrors,
                  routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual(),
                  true
                )
              ),
            reasonForClaimAndBasis => {
              val updatedAnswers: ReasonAndBasisOfClaimAnswer = answers.fold(
                _ => CompleteReasonAndBasisOfClaimAnswer(reasonForClaimAndBasis),
                complete => complete.copy(selectReasonForBasisAndClaim = reasonForClaimAndBasis)
              )
              val newDraftClaim                               =
                fillingOutClaim.draftClaim.fold(_.copy(reasonForBasisAndClaimAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not store reason for reason and basis answer", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
              )
            }
          )
      }

    }
}

object SelectReasonForBasisAndClaimController {

  final case class SelectReasonForClaimAndBasis(
    basisForClaim: BasisOfClaim,
    reasonForClaim: ReasonForClaim
  )

  object SelectReasonForClaimAndBasis {
    implicit val format: OFormat[SelectReasonForClaimAndBasis] = Json.format[SelectReasonForClaimAndBasis]
  }

  val reasonForClaimForm: Form[SelectReasonForClaimAndBasis] =
    Form(
      mapping(
        "select-reason-and-basis-for-claim.basis"  -> number
          .verifying(
            "invalid basis for claim",
            reason => BasisOfClaim.intValues.contains(reason)
          )
          .transform[BasisOfClaim](int => BasisOfClaim.int2ClaimMap(int), claim => BasisOfClaim.claim2IntMap(claim)),
        "select-reason-and-basis-for-claim.reason" -> number
          .verifying(
            "invalid basis for reason",
            reason =>
              reason === 0 ||
                reason === 1 ||
                reason === 2 ||
                reason === 3
          )
          .transform[ReasonForClaim](
            {
              case 0 => ReasonForClaim.MailForOrderGoods
              case 1 => ReasonForClaim.Overpayment
              case 2 => ReasonForClaim.SpecialGoods
            },
            {
              case ReasonForClaim.MailForOrderGoods => 0
              case ReasonForClaim.Overpayment       => 1
              case ReasonForClaim.SpecialGoods      => 2
            }
          )
      )(SelectReasonForClaimAndBasis.apply)(SelectReasonForClaimAndBasis.unapply)
    )

}
