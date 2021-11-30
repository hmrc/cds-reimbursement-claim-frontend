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
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterCommoditiesDetailsController.commoditiesDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.CommodityDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class EnterCommoditiesDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterCommoditiesDetailsPage: pages.enter_commodities_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with Logging
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[CommodityDetailsAnswer] = _.commoditiesDetailsAnswer

  def enterCommoditiesDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[CommodityDetailsAnswer] { (_, answers) =>
        val form = answers.toList.foldLeft(commoditiesDetailsForm)((form, answer) => form.fill(answer))
        Ok(enterCommoditiesDetailsPage(form))
      }
    }

  def enterCommoditiesDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[CommodityDetailsAnswer] { (fillingOutClaim, _, router) =>
        import router._

        commoditiesDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors => BadRequest(enterCommoditiesDetailsPage(requestFormWithErrors)),
            commodityDetails => {
              val newDraftClaim  = fillingOutClaim.draftClaim.copy(commoditiesDetailsAnswer = Some(commodityDetails))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("could not update session"))
                .fold(
                  logAndDisplayError("could not get commodity details"),
                  _ =>
                    Redirect(
                      CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively = journeyBindable match {
                        case JourneyBindable.Scheduled =>
                          claimRoutes.SelectDutyTypesController.showDutyTypes()
                        case JourneyBindable.Multiple  =>
                          claimRoutes.SelectMultipleDutiesController.selectDuties(index = 1)
                        case _                         =>
                          claimRoutes.SelectDutiesController.selectDuties()
                      })
                    )
                )
            }
          )
      }
    }
}

object EnterCommoditiesDetailsController {

  val commoditiesDetailsForm: Form[CommodityDetailsAnswer] = Form(
    mapping("enter-commodities-details" -> nonEmptyText(maxLength = 500))(CommodityDetailsAnswer.apply)(
      CommodityDetailsAnswer.unapply
    )
  )
}
