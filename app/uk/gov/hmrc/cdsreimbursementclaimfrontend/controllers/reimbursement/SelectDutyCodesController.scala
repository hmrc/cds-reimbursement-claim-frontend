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
import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{list, mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.SelectDutyCodesController.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType.{CiderPerry, EuDuty, UkDuty}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyCodesAnswer, DutyType, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, TaxCode, reimbursement}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class SelectDutyCodesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  selectDutyCodesPage: pages.select_duty_codes
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[DutyCodesAnswer] = _.dutyCodesSelectedAnswer

  def start(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[DutyCodesAnswer] { (fillingOutClaim, answer) =>
      fillingOutClaim.draftClaim.dutyTypesSelectedAnswer
        .fold(
          Future.successful(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))
        ) { dutyTypesAnswer =>
          val dutyCodesAnswer = answer.getOrElse(DutyCodesAnswer.none)

          val updatedReimbursementState = ReimbursementState.computeReimbursementState(
            dutyTypesAnswer,
            dutyCodesAnswer
          )

          val updatedJourneyStatus = FillingOutClaim.of(fillingOutClaim)(
            _.copy(
              dutyTypesSelectedAnswer = Some(updatedReimbursementState.dutyTypesAnswer),
              dutyCodesSelectedAnswer = Some(updatedReimbursementState.dutyCodesAnswer)
            )
          )

          EitherT(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourneyStatus))))
            .leftMap(_ => Error("could not update session"))
            .fold(
              logAndDisplayError("could not update reimbursement state"),
              _ =>
                (
                  updatedJourneyStatus.draftClaim.dutyTypesSelectedAnswer,
                  updatedJourneyStatus.draftClaim.dutyCodesSelectedAnswer
                ) match {
                  case (Some(_), Some(dutyCodesAnswer)) =>
                    dutyCodesAnswer.existsDutyTypeWithNoDutyCodesAnswer match {
                      case Some(dutyType) =>
                        Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType))
                      case None           =>
                        Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursementClaim())
                      //Redirect(reimbursementRoutes.EnterReimbursementClaimController.start())
                    }
                  case _                                =>
                    logger.warn("could not find duty types or duty codes")
                    errorHandler.errorResult()
                }
            )
        }
    }
  }

  def showDutyCodes(dutyType: DutyType): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withAnswers[DutyCodesAnswer] { (_, answer) =>
        answer.fold(
          Ok(selectDutyCodesPage(SelectDutyCodesController.selectDutyCodesForm(dutyType), dutyType))
        )(dutyCodesAnswer =>
          Ok(
            selectDutyCodesPage(
              SelectDutyCodesController.selectDutyCodesForm(dutyType).fill(dutyCodesAnswer),
              dutyType
            )
          )
        )
      }
  }

  def submitDutyCodes(dutyType: DutyType): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswers[DutyCodesAnswer] { (fillingOutClaim, maybeCurrentAnswer) =>
        selectDutyCodesForm(dutyType)
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectDutyCodesPage(formWithErrors, dutyType)),
            selectedAnswer => updateSessionCache(dutyType, selectedAnswer, maybeCurrentAnswer, fillingOutClaim)
          )
      }
    }

  private def updateSessionCache(
    dutyType: DutyType,
    dutyTypesSelectedAnswer: DutyCodesAnswer,
    currentAnswer: Option[DutyCodesAnswer],
    fillingOutClaim: FillingOutClaim
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[AnyContent]): Future[Result] = {

    val updatedDutyTypeToDutyCodesMap: Map[DutyType, List[TaxCode]] =
      currentAnswer.fold(Map[reimbursement.DutyType, List[TaxCode]]()) { dutyCodesAnswer =>
        dutyCodesAnswer.dutyCodes ++ List(dutyType -> dutyTypesSelectedAnswer.dutyCodes(dutyType))
      }

    val updatedJourney =
      FillingOutClaim.of(fillingOutClaim)(
        _.copy(
          dutyCodesSelectedAnswer = Some(DutyCodesAnswer(updatedDutyTypeToDutyCodesMap)),
          reimbursementClaimAnswer = Some(
            ReimbursementClaimAnswer(
              Map(
                UkDuty     -> Map(
                  TaxCode.A20 -> ReimbursementClaim(paidAmount = Some(600.00), shouldOfPaid = Some(300.00)),
                  TaxCode.A00 -> ReimbursementClaim(paidAmount = Some(1000.00), shouldOfPaid = Some(400.00))
                ),
                EuDuty     -> Map(
                  TaxCode.A70 -> ReimbursementClaim(paidAmount = Some(600.00), shouldOfPaid = Some(300.00)),
                  TaxCode.A00 -> ReimbursementClaim(paidAmount = Some(1000.00), shouldOfPaid = Some(400.00))
                ),
                CiderPerry -> Map(
                  TaxCode.NI431 -> ReimbursementClaim(paidAmount = Some(600.00), shouldOfPaid = Some(300.00))
                )
              )
            )
          )
        )
      )

    EitherT
      .liftF(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap((_: Unit) => Error("could not update session"))
      .fold(
        logAndDisplayError("could not get duty types selected"),
        _ =>
          updatedJourney.draftClaim.dutyCodesSelectedAnswer match {
            case Some(dutyCodesAnswer) =>
              dutyCodesAnswer.existsDutyTypeWithNoDutyCodesAnswer match {
                case Some(dutyType) =>
                  Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType))
                case None           =>
                  Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursementClaim())
//                  Redirect(reimbursementRoutes.EnterReimbursementClaimController.start())
              }
            case None                  =>
              logger.warn("could not find duty codes")
              errorHandler.errorResult()
          }
      )
  }
}

object SelectDutyCodesController {

  def selectDutyCodesForm(dutyType: DutyType): Form[DutyCodesAnswer] =
    Form(
      mapping(
        "select-duty-codes" -> list(
          mapping(
            "" -> nonEmptyText
              .verifying(
                "invalid duty code",
                code => TaxCode.allTaxCodes.map(_.value).exists(_ === code)
              )
          )(TaxCode.apply)(TaxCode.unapply)
        ).verifying("error.required", _.nonEmpty)
      )(selectedDutyCodes => DutyCodesAnswer(Map(dutyType -> selectedDutyCodes)))(dutyCodesAnswer =>
        dutyCodesAnswer.dutyCodes.values.headOption
      )
    )
}
