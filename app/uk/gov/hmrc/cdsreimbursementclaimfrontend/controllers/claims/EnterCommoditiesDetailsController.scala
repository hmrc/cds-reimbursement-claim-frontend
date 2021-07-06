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
import cats.implicits.catsSyntaxOptionId
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{CommodityDetails, DraftClaim, Error, SessionData, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

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
    with Logging
    with SessionUpdates {

  private def withCommoditiesDetails(
    f: (
      SessionData,
      FillingOutClaim,
      Option[CommodityDetails]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (sessionData, fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
      draftClaim
        .fold(_.commoditiesDetailsAnswer)
        .map(details => f(sessionData, fillingOutClaim, details.some))
        .getOrElse(f(sessionData, fillingOutClaim, None))
    })

  def enterCommoditiesDetails: Action[AnyContent]  = show(isAmend = false)
  def changeCommoditiesDetails: Action[AnyContent] = show(isAmend = true)

  def show(isAmend: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCommoditiesDetails { (_, _, answers) =>
        val commoditiesDetailsForm =
          answers.toList.foldLeft(EnterCommoditiesDetailsController.commoditiesDetailsForm)((form, answer) =>
            form.fill(answer)
          )
        Ok(enterCommoditiesDetailsPage(commoditiesDetailsForm, isAmend))
      }
    }

  def enterCommoditiesDetailsSubmit: Action[AnyContent]  =
    submit(isAmend = false, routes.SelectDutiesController.selectDuties())
  def changeCommoditiesDetailsSubmit: Action[AnyContent] =
    submit(isAmend = true, routes.CheckYourAnswersAndSubmitController.checkAllAnswers())

  def submit(isAmend: Boolean, redirectUrl: Call): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCommoditiesDetails { (_, fillingOutClaim, _) =>
        EnterCommoditiesDetailsController.commoditiesDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterCommoditiesDetailsPage(
                  requestFormWithErrors,
                  isAmend
                )
              ),
            commodityDetails => {
              val updatedJourney =
                FillingOutClaim.of(fillingOutClaim)(_.fold(_.copy(commoditiesDetailsAnswer = Some(commodityDetails))))

              EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))
                .fold(
                  e => {
                    logger.warn("could not get commodity details", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(redirectUrl)
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
