/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.data.OptionT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.list
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectDutyCodesController.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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
      Future.successful(Redirect(claimRoutes.SelectDutyTypesController.showDutyTypes()))

    def start(dutyType: DutyType): Future[Result] =
      Future(Redirect(claimRoutes.SelectDutyCodesController.showDutyCodes(dutyType)))

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
                        .fold(Redirect(claimRoutes.EnterScheduledClaimController.iterate()))(nextDuty =>
                          Redirect(claimRoutes.SelectDutyCodesController.showDutyCodes(nextDuty))
                        )
                  )
                )
                .getOrElse(Redirect(claimRoutes.SelectDutyTypesController.showDutyTypes()))
          )
      }
    }
}

object SelectDutyCodesController {

  val selectDutyCodesKey: String = "select-duty-codes"

  val selectDutyCodesForm: Form[List[TaxCode]] =
    Form(
      mapping(
        selectDutyCodesKey -> list(
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
