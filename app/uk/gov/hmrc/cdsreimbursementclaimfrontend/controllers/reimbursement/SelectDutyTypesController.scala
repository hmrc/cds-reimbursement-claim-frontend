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

/*
    Show the full list of categories a user can choose from
    Store selection
    Process changes to selections compared to what is in the session store
      - if deselection, then remove the sub-categories and associated claim amounts from session cache
      - if new selections, then works as normal, go to next page which will work out which type has no sub-types selected


      Correct URL: scheduled/select-duties/select-duty-types
 */

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{list, mapping, number}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.SelectDutyTypesController.selectDutyTypesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutyTypesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType
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
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  selectDutyTypesPage: pages.select_duty_types
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[DutyTypesSelectedAnswer] = _.dutyTypesSelectedAnswer

  def showDutyTypes(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[DutyTypesSelectedAnswer] { (fillingOutClaim, _) =>
      fillingOutClaim.draftClaim
        .fold(_.dutyTypesSelectedAnswer)
        .fold(
          Ok(
            selectDutyTypesPage(
              selectDutyTypesForm
            )
          )
        )(dutyType =>
          Ok(
            selectDutyTypesPage(
              selectDutyTypesForm
                .fill(dutyType)
            )
          )
        )
    }
  }

  def submitDutyTypes(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswers[DutyTypesSelectedAnswer] { (fillingOutClaim, previousAnswer) =>
        selectDutyTypesForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectDutyTypesPage(formWithErrors)),
            selectedAnswer =>
              previousAnswer.fold {
                updateDutyTypeAnswer(selectedAnswer, fillingOutClaim)
              }(pa =>
                if (selectedAnswer === pa) {
                  Redirect(reimbursementRoutes.SelectDutySubTypesController.start())
                } else {
                  updateDutyTypeAnswer(selectedAnswer, fillingOutClaim)
                }
              )
          )
      }
    }

  private def updateDutyTypeAnswer(
    dutyTypesSelectedAnswer: DutyTypesSelectedAnswer,
    fillingOutClaim: FillingOutClaim
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[AnyContent]): Future[Result] = {
    val newDraftClaim: DraftC285Claim   =
      fillingOutClaim.draftClaim.fold(_.copy(dutyTypesSelectedAnswer = Some(dutyTypesSelectedAnswer)))
    val updatedJourney: FillingOutClaim = fillingOutClaim.copy(draftClaim = newDraftClaim)

    EitherT
      .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap((_: Unit) => Error("could not update session"))
      .fold(
        logAndDisplayError("could not get duty types selected"),
        _ => Redirect(reimbursementRoutes.SelectDutySubTypesController.start())
      )
  }

}

object SelectDutyTypesController {

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def selectDutyTypesForm: Form[DutyTypesSelectedAnswer] = Form(
    mapping(
      "select-duty-types" -> list(
        mapping(
          "" -> number
        )(DutyType.apply)(DutyType.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(listOfDutyTypes => DutyTypesSelectedAnswer(listOfDutyTypes.head, listOfDutyTypes.tail: _*))(
      nonEmptyListOfDutyTypes => Some(nonEmptyListOfDutyTypes.toList)
    )
  )

}
