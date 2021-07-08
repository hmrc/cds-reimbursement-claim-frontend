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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{CommodityDetails, Error, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Serg._

import scala.concurrent.ExecutionContext

@Singleton
class EnterCommoditiesDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterCommoditiesDetailsPage: pages.enter_commodities_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with Logging
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[CommodityDetails] = _.commoditiesDetailsAnswer

  def enterCommoditiesDetails(implicit journey: JourneyBindable): Action[AnyContent]  = show(isAmend = false)
  def changeCommoditiesDetails(implicit journey: JourneyBindable): Action[AnyContent] = show(isAmend = true)

  def show(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[CommodityDetails] { (_, answers, router) =>
        val commoditiesDetailsForm =
          answers.toList.foldLeft(EnterCommoditiesDetailsController.commoditiesDetailsForm)((form, answer) =>
            form.fill(answer)
          )
        Ok(enterCommoditiesDetailsPage(commoditiesDetailsForm, router, isAmend))
      }
    }

  def enterCommoditiesDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent]  =
    submit(isAmend = false)
  def changeCommoditiesDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    submit(isAmend = true)

  def submit(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[CommodityDetails] { (fillingOutClaim, _, router) =>
        EnterCommoditiesDetailsController.commoditiesDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterCommoditiesDetailsPage(
                  requestFormWithErrors,
                  router,
                  isAmend
                )
              ),
            commodityDetails => {
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(commoditiesDetailsAnswer = Some(commodityDetails)))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))
                .fold(
                  e => {
                    logger.warn("could not get commodity details", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(router.nextPage(isAmend))
                )
            }
          )
      }
    }
}

object EnterCommoditiesDetailsController {

  val commoditiesDetailsForm: Form[CommodityDetails] = Form(
    mapping(
      "enter-commodities-details" -> nonEmptyText(maxLength = 500)
    )(CommodityDetails.apply)(CommodityDetails.unapply)
  )
}
