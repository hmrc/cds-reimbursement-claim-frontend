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

import cats.data.OptionT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{list, mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.SelectDutyCodesController.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutyType, TaxCode, TaxCodes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class SelectDutyCodesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  selectDutyCodesPage: pages.select_duty_codes
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[SelectedDutyTaxCodesReimbursementAnswer] =
    _.selectedDutyTaxCodesReimbursementAnswer

  def iterate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    def selectDuties: Future[Result] =
      Future.successful(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))

    def start(dutyType: DutyType): Future[Result] =
      Future(Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType)))

    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (_, maybeAnswer) =>
      maybeAnswer.flatMap(_.value.headOption).fold(selectDuties) { selectedDutyTaxCodesReimbursement =>
        start(selectedDutyTaxCodesReimbursement._1)
      }
    }
  }

  def showDutyCodes(dutyType: DutyType): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (_, maybeAnswer) =>
        val maybeSelectedTaxCodes = maybeAnswer.map(_.getTaxCodes(dutyType))

        Ok(
          selectDutyCodesPage(dutyType, maybeSelectedTaxCodes.toList.foldLeft(selectDutyCodesForm)(_.fill(_)))
        )
      }
  }

  def submitDutyCodes(currentDuty: DutyType): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
        def updateClaim(answer: SelectedDutyTaxCodesReimbursementAnswer) = {
          val claim = from(fillingOutClaim)(_.copy(selectedDutyTaxCodesReimbursementAnswer = answer.some))
          updateSession(sessionCache, request)(_.copy(journeyStatus = claim.some))
        }

        selectDutyCodesForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectDutyCodesPage(currentDuty, formWithErrors)),
            selectedTaxCodes =>
              OptionT
                .fromOption[Future](maybeAnswer.map(_.reapply(selectedTaxCodes)(currentDuty)))
                .semiflatMap(updateClaim)
                .map(
                  _.fold(
                    logAndDisplayError("Error updating tax codes selection: "),
                    _ =>
                      maybeAnswer
                        .flatMap(_.findNextSelectedDutyAfter(currentDuty))
                        .fold(Redirect(reimbursementRoutes.EnterReimbursementClaimController.iterate()))(nextDuty =>
                          Redirect(reimbursementRoutes.SelectDutyCodesController.showDutyCodes(nextDuty))
                        )
                  )
                )
                .getOrElse(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))
          )
      }
    }
}

object SelectDutyCodesController {

  val selectDutyCodesForm: Form[List[TaxCode]] =
    Form(
      mapping(
        "select-duty-codes" -> list(
          mapping(
            "" -> nonEmptyText
              .verifying(
                "error.invalid",
                code => TaxCodes has code
              )
          )(TaxCode.apply)(TaxCode.unapply)
        ).verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )
}
