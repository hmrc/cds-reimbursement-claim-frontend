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
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

/*

    Given the selected duties types in the session I need to find all the associated codes for each one
    and then display each duty type with their associated code.

    There must be at least one selected duty code from each category

    Once all the duty types have had at least one duty code selected we move onto the financial claim amounts page

    If the user goes back and modifies their duty type selection then they list of associated codes are updated (complex logic around this)






 */

class SelectDutyCodesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration
//  selectDutySubTypesPage: pages.select_duty_codes
)(implicit ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[DutyCodesAnswer] = _.dutyCodesSelectedAnswer

  def start(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[DutyCodesAnswer] { (fillingOutClaim, answer) =>
      fillingOutClaim.draftClaim
        .fold(_.dutyTypesSelectedAnswer)
        .fold(
          Future.successful(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))
        ) { dutyTypesAnswer =>
          val updatedJourneyStatus: FillingOutClaim = answer.fold(
            fillingOutClaim
          ) { dutyCodesAnswer =>
            val updatedReimbursementState = ReimbursementState.computeReimbursementState(
              dutyTypesAnswer,
              dutyCodesAnswer
            )

            FillingOutClaim.of(fillingOutClaim)(
              _.copy(
                dutyTypesSelectedAnswer = Some(updatedReimbursementState.dutyTypesAnswer),
                dutyCodesSelectedAnswer = Some(updatedReimbursementState.dutyCodesAnswer)
              )
            )
          }

          EitherT
            .liftF(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourneyStatus))))
            .leftMap((_: Unit) => Error("could not update session"))
            .fold(
              logAndDisplayError("could not update reimbursement state"),
              _ =>
                updatedJourneyStatus.draftClaim
                  .fold(_.dutyCodesSelectedAnswer)
                  .fold {
                    logger.info("could not find duty codes")
                    errorHandler.errorResult()
                  }(dutyCodes =>
                    dutyCodes.dutyCodes.find(dutyTypeToDutyCode => dutyTypeToDutyCode._2.isEmpty) match {
                      case Some(dutyTypeToDutyCode) =>
                        Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyTypeToDutyCode._1))
                      case None                     => Redirect(reimbursementRoutes.EnterPaidAndClaimAmountController.start())
                    }
                  )
            )
        }
    }
  }

  def showDutyCodes(dutyType: DutyType): Action[AnyContent] = Action {
    Ok(s"show $dutyType")
  }

  def submitDutyCodes(dutyType: DutyType): Action[AnyContent] = Action {
    Ok(s"submitted: $dutyType")
  }

}

object SelectDutyCodesController {}
