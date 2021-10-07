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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.data.EitherT
import cats.implicits._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyCodesAnswer, DutyType, ReimbursementClaim, ReimbursementClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, TaxCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class EnterReimbursementClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterReimbursementClaimPage: pages.enter_reimbursement_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[ReimbursementClaimAnswer] = _.reimbursementClaimAnswer

  /*
    This should read the current value for duty codes answer and build a reimbursementClaim map first
    only if one does not currently exist in the session otherwise the previous data will trashed
    if one does exist then we need to do some surgenry and work out which claims need to be removed from
    the map and which ones need to added and initalised to None ready to be picked up again
   */
  def start(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[ReimbursementClaimAnswer] { (fillingOutClaim, answer) =>
      fillingOutClaim.draftClaim
        .fold(_.dutyCodesSelectedAnswer)
        .fold(
          Future.successful(Redirect(reimbursementRoutes.SelectDutyCodesController.start()))
        ) { dutyCodesAnswer =>
          val updatedJourneyStatus: FillingOutClaim = answer.fold {
            updateJourneyStatus(dutyCodesAnswer, ReimbursementClaimAnswer.initialise(dutyCodesAnswer), fillingOutClaim)
          } { reimbursementClaimAnswer =>
            updateJourneyStatus(dutyCodesAnswer, reimbursementClaimAnswer, fillingOutClaim)
          }

          EitherT
            .liftF(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourneyStatus))))
            .leftMap((_: Unit) => Error("could not update session"))
            .fold(
              logAndDisplayError("could not update reimbursement claims"),
              _ =>
                (
                  updatedJourneyStatus.draftClaim.fold(_.dutyTypesSelectedAnswer),
                  updatedJourneyStatus.draftClaim.fold(_.dutyCodesSelectedAnswer)
                ) match {
                  case (Some(_), Some(dutyCodesAnswer: DutyCodesAnswer)) =>
                    dutyCodesAnswer.existsDutyTypeWithNoDutyCodesAnswer match {
                      case Some(dutyType) =>
                        Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType))
                      case None           => Redirect(reimbursementRoutes.EnterReimbursementClaimController.start())
                    }
                  case _                                                 =>
                    logger.warn("could not find duty types or duty codes")
                    errorHandler.errorResult()
                }
            )
        }
    }
  }

  private def updateJourneyStatus(
    dutyCodesAnswer: DutyCodesAnswer,
    reimbursementClaimAnswer: ReimbursementClaimAnswer,
    fillingOutClaim: FillingOutClaim
  ): FillingOutClaim =
    FillingOutClaim.of(fillingOutClaim)(
      _.copy(
        reimbursementClaimAnswer = Some(reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer))
      )
    )

  def showReimbursementClaim(dutyType: DutyType, dutyCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ReimbursementClaimAnswer] { (_, answer) =>
        answer.fold(
          Ok(
            enterReimbursementClaimPage(
              EnterReimbursementClaimController.enterReimbursementClaimForm,
              dutyType,
              dutyCode
            )
          )
        )(reimbursementClaimAnswer =>
          Ok(
            enterReimbursementClaimPage(
              EnterReimbursementClaimController.enterReimbursementClaimForm.fill(
                reimbursementClaimAnswer.reimbursementClaims(dutyType)(dutyCode)
              ),
              dutyType,
              dutyCode
            )
          )
        )
      }
    }

  def submitReimbursementClaim(dutyType: DutyType, dutyCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswers[ReimbursementClaimAnswer] { (fillingOutClaim, maybeReimbursementClaimAnswer) =>
        EnterReimbursementClaimController.enterReimbursementClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterReimbursementClaimPage(formWithErrors, dutyType, dutyCode)),
            reimbursementClaim =>
              maybeReimbursementClaimAnswer
                .fold(
                  Future.successful(
                    Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursementClaim())
                  )
                )(reimbursementClaimAnswer =>
                  updateSessionCache(
                    dutyType,
                    dutyCode,
                    reimbursementClaim,
                    reimbursementClaimAnswer,
                    fillingOutClaim
                  )
                )
          )
      }
    }

  private def updateSessionCache(
    dutyType: DutyType,
    dutyCode: TaxCode,
    reimbursementClaim: ReimbursementClaim,
    reimbursementClaimAnswer: ReimbursementClaimAnswer,
    fillingOutClaim: FillingOutClaim
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[AnyContent]): Future[Result] = {

    val updatedReimbursementClaimAnswer: ReimbursementClaimAnswer =
      reimbursementClaimAnswer.updateReimbursementClaim(dutyType, dutyCode, reimbursementClaim)

    val updatedJourney =
      FillingOutClaim.of(fillingOutClaim)(
        _.copy(reimbursementClaimAnswer = Some(updatedReimbursementClaimAnswer))
      )

    EitherT
      .liftF(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap((_: Unit) => Error("could not update session"))
      .fold(
        logAndDisplayError("could not get duty types selected"),
        _ =>
          updatedJourney.draftClaim.fold(_.reimbursementClaimAnswer) match {
            case Some(reimbursementClaimAnswer) =>
              reimbursementClaimAnswer.isIncompleteReimbursementClaim match {
                case Some(dutyType) =>
                  Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType._1))
                case None           => Redirect(reimbursementRoutes.EnterReimbursementClaimController.start())
              }
            case None                           =>
              logger.warn("could not find duty codes")
              errorHandler.errorResult()
          }
      )
  }

}

object EnterReimbursementClaimController {

  def enterReimbursementClaimForm: Form[ReimbursementClaim] = Form(
    mapping(
      "enter-reimbursement-claim.amount-paid"           -> moneyMapping(13, 2, "amount-paid.error.invalid")
        .transform[Option[BigDecimal]](
          amount => Some(amount),
          {
            case Some(paidAmount) => paidAmount
            case None             => sys.error("could not get paid amount")
          }
        ),
      "enter-reimbursement-claim.amount-should-of-paid" -> moneyMapping(13, 2, "amount-should-of-paid.error.invalid")
        .transform[Option[BigDecimal]](
          amount => Some(amount),
          {
            case Some(shouldOfPaid) => shouldOfPaid
            case None               => sys.error("could not get amount should of paid")
          }
        )
    )(ReimbursementClaim.apply)(ReimbursementClaim.unapply)
      .verifying(
        "invalid.reimbursement-claim",
        reimbursementClaim => reimbursementClaim.paidAmount <= reimbursementClaim.shouldOfPaid
      )
    //TODO: add other validation
  )

}
