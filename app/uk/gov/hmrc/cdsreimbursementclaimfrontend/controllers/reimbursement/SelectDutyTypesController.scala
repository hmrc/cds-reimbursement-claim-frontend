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
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{list, mapping, nonEmptyText}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.SelectDutyTypesController.selectDutyTypesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyType, DutyTypesAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
@Singleton
class SelectDutyTypesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  selectDutyTypesPage: pages.select_duty_types
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[DutyTypesAnswer] = _.dutyTypesSelectedAnswer

  def showDutyTypes(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[DutyTypesAnswer] { (_, answer) =>
      answer.fold(
        Ok(selectDutyTypesPage(selectDutyTypesForm))
      )(dutyType => Ok(selectDutyTypesPage(selectDutyTypesForm.fill(dutyType))))
    }
  }

  def submitDutyTypes(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswers[DutyTypesAnswer] { (fillingOutClaim, answer) =>
        selectDutyTypesForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectDutyTypesPage(formWithErrors)),
            selectedAnswer =>
              answer.fold {
                updateDutyTypeAnswer(selectedAnswer, fillingOutClaim)
              }(dutyTypesSelectedAnswer =>
                if (selectedAnswer === dutyTypesSelectedAnswer) {
                  Redirect(reimbursementRoutes.SelectDutyCodesController.start())
                } else {
                  updateDutyTypeAnswer(selectedAnswer, fillingOutClaim)
                }
              )
          )
      }
    }

  private def updateDutyTypeAnswer(
    dutyTypesSelectedAnswer: DutyTypesAnswer,
    fillingOutClaim: FillingOutClaim
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[AnyContent]): Future[Result] = {
    val updatedJourney =
      FillingOutClaim.of(fillingOutClaim)(_.copy(dutyTypesSelectedAnswer = Some(dutyTypesSelectedAnswer)))
    EitherT
      .liftF(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap((_: Unit) => Error("could not update session"))
      .fold(
        logAndDisplayError("could not get duty types selected"),
        _ => Redirect(reimbursementRoutes.SelectDutyCodesController.start())
      )
  }

}

object SelectDutyTypesController {

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def selectDutyTypesForm: Form[DutyTypesAnswer] = Form(
    mapping(
      "select-duty-types" -> list(
        mapping(
          "" -> nonEmptyText
            .verifying(
              "invalid duty type code",
              code => DutyType.dutyTypes.map(_.repr).exists(_ === code)
            )
        )(DutyType.apply)(DutyType.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(selectedDutyTypes => DutyTypesAnswer(selectedDutyTypes))(dutyTypesAnswer =>
      Some(dutyTypesAnswer.dutyTypesSelected)
    )
  )

}
