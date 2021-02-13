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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForClaimAnswer.IncompleteReasonForClaimAnswer
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
      ReasonForClaimAnswer
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
          _.reasonForClaim
        )
        maybeReasonForClaim.fold[Future[Result]](
          f(s, r, IncompleteReasonForClaimAnswer.empty)
        )(f(s, r, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def selectReasonForClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectReasonForClaim { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.reasonForClaimOption match {
              case Some(reasonForClaimOption) =>
                Ok(
                  selectReasonForClaimPage(
                    SelectReasonForClaimController.reasonForClaimForm.fill(SelectReasonForClaim(reasonForClaimOption)),
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual
                  )
                )
              case None                       =>
                Ok(
                  selectReasonForClaimPage(
                    SelectReasonForClaimController.reasonForClaimForm,
                    routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual
                  )
                )
            },
          ifComplete =>
            Ok(
              selectReasonForClaimPage(
                SelectReasonForClaimController.reasonForClaimForm.fill(
                  SelectReasonForClaim(ifComplete.reasonForClaimOption)
                ),
                routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual
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
                  routes.EnterClaimantDetailsAsIndividualController.enterClaimantDetailsAsIndividual
                )
              ),
            reasonForClaim => {
              val updatedAnswers: ReasonForClaimAnswer = answers.fold(
                incomplete => incomplete.copy(reasonForClaimOption = Some(reasonForClaim.reasonForClaimOption)),
                complete => complete.copy(reasonForClaimOption = reasonForClaim.reasonForClaimOption)
              )
              val newDraftClaim                        =
                fillingOutClaim.draftClaim.fold(_.copy(reasonForClaim = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not store reason for claim answer", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence())
              )
            }
          )
      }

    }
}

object SelectReasonForClaimController {

  final case class SelectReasonForClaim(
    reasonForClaimOption: ReasonForClaimOption
  )

  val reasonForClaimForm: Form[SelectReasonForClaim] =
    Form(
      mapping(
        "select-reason-for-claim" -> number
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
          .transform[ReasonForClaimOption](
            {
              case 0  => ReasonForClaimOption.DuplicateMrnEntry
              case 1  => ReasonForClaimOption.DutySuspension
              case 2  => ReasonForClaimOption.EndUseRelief
              case 3  => ReasonForClaimOption.IncorrectCommodityCode
              case 4  => ReasonForClaimOption.IncorrectCpc
              case 5  => ReasonForClaimOption.IncorrectValue
              case 6  => ReasonForClaimOption.IncorrectEoriAndDefermentAccountNumber
              case 7  => ReasonForClaimOption.InwardProcessingReliefFromCustomsDuty
              case 8  => ReasonForClaimOption.OutwardProcessingRelief
              case 9  => ReasonForClaimOption.Preference
              case 10 => ReasonForClaimOption.ProofOfReturnRefundGiven
            },
            {
              case ReasonForClaimOption.DuplicateMrnEntry                      => 0
              case ReasonForClaimOption.DutySuspension                         => 1
              case ReasonForClaimOption.EndUseRelief                           => 2
              case ReasonForClaimOption.IncorrectCommodityCode                 => 3
              case ReasonForClaimOption.IncorrectCpc                           => 4
              case ReasonForClaimOption.IncorrectValue                         => 5
              case ReasonForClaimOption.IncorrectEoriAndDefermentAccountNumber => 6
              case ReasonForClaimOption.InwardProcessingReliefFromCustomsDuty  => 7
              case ReasonForClaimOption.OutwardProcessingRelief                => 8
              case ReasonForClaimOption.Preference                             => 9
              case ReasonForClaimOption.ProofOfReturnRefundGiven               => 10
            }
          )
      )(SelectReasonForClaim.apply)(SelectReasonForClaim.unapply)
    )

}
