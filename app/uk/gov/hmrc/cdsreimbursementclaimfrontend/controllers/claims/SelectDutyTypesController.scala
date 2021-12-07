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
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{list, mapping, nonEmptyText}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectDutyTypesController.selectDutyTypesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutyType, DutyTypes, Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectDutyTypesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  selectDutyTypesPage: pages.select_duty_types
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[SelectedDutyTaxCodesReimbursementAnswer] =
    _.selectedDutyTaxCodesReimbursementAnswer

  def showDutyTypes(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (_, answer) =>
      Ok(
        selectDutyTypesPage(
          answer.map(_.value.keys.toList).fold(selectDutyTypesForm)(selectDutyTypesForm.fill)
        )
      )
    }
  }

  def submitDutyTypes(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
        selectDutyTypesForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectDutyTypesPage(formWithErrors)),
            selectedDuties => {

              val previousAnswer = maybeAnswer getOrElse SelectedDutyTaxCodesReimbursementAnswer.none

              val updatedAnswer =
                SelectedDutyTaxCodesReimbursementAnswer buildFrom selectedDuties synchronizingWith previousAnswer

              val updatedJourney =
                FillingOutClaim
                  .from(fillingOutClaim)(_.copy(selectedDutyTaxCodesReimbursementAnswer = Some(updatedAnswer)))

              EitherT(updateSession(sessionCache, request)(_.copyWith(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("could not update session"))
                .fold(
                  logAndDisplayError("could not get duty types selected"),
                  _ => Redirect(claimRoutes.SelectDutyCodesController.iterate())
                )
            }
          )
      }
    }
}

object SelectDutyTypesController {

  lazy val selectDutyTypesForm: Form[List[DutyType]] = Form(
    mapping(
      "select-duty-types" -> list(
        mapping(
          "" -> nonEmptyText
            .verifying(
              "error.invalid",
              code => DutyTypes has code
            )
        )(DutyType.apply)(DutyType.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(identity)(Some(_))
  )
}
