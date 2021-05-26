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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CommoditiesDetailsAnswer.{CompleteCommodityDetailsAnswer, IncompleteCommoditiesDetailsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{CommoditiesDetailsAnswer, CommodityDetails, DraftClaim, Error, SessionData, upscan => _}
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
      CommoditiesDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeCommoditiesDetailsAnswers = draftClaim.fold(
          _.commoditiesDetailsAnswer
        )
        maybeCommoditiesDetailsAnswers.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteCommoditiesDetailsAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterCommoditiesDetails: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCommoditiesDetails { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.commodityDetails match {
              case Some(commodityDetails) =>
                Ok(
                  enterCommoditiesDetailsPage(
                    EnterCommoditiesDetailsController.commoditiesDetailsForm.fill(commodityDetails)
                  )
                )
              case None                   =>
                Ok(
                  enterCommoditiesDetailsPage(
                    EnterCommoditiesDetailsController.commoditiesDetailsForm
                  )
                )
            },
          ifComplete =>
            Ok(
              enterCommoditiesDetailsPage(
                EnterCommoditiesDetailsController.commoditiesDetailsForm.fill(
                  ifComplete.commodityDetails
                )
              )
            )
        )
      }
    }

  //TODO; error check on character count
  def enterCommoditiesDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCommoditiesDetails { (_, fillingOutClaim, answers) =>
        EnterCommoditiesDetailsController.commoditiesDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterCommoditiesDetailsPage(
                  requestFormWithErrors
                )
              ),
            commodityDetails => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteCommodityDetailsAnswer(
                    commodityDetails
                  ),
                complete => complete.copy(commodityDetails = commodityDetails)
              )

              val newDraftClaim =
                fillingOutClaim.draftClaim.fold(_.copy(commoditiesDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not get commodity details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.SelectDutiesController.selectDuties())
              )
            }
          )
      }
    }

  def changeCommoditiesDetails: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCommoditiesDetails { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.commodityDetails match {
              case Some(commodityDetails) =>
                Ok(
                  enterCommoditiesDetailsPage(
                    EnterCommoditiesDetailsController.commoditiesDetailsForm.fill(commodityDetails),
                    isAmend = true
                  )
                )
              case None                   =>
                Ok(
                  enterCommoditiesDetailsPage(
                    EnterCommoditiesDetailsController.commoditiesDetailsForm,
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              enterCommoditiesDetailsPage(
                EnterCommoditiesDetailsController.commoditiesDetailsForm.fill(
                  ifComplete.commodityDetails
                ),
                true
              )
            )
        )
      }
    }

  //TODO; error check on character count
  def changeCommoditiesDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCommoditiesDetails { (_, fillingOutClaim, answers) =>
        EnterCommoditiesDetailsController.commoditiesDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterCommoditiesDetailsPage(
                  requestFormWithErrors,
                  true
                )
              ),
            commodityDetails => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteCommodityDetailsAnswer(
                    commodityDetails
                  ),
                complete => complete.copy(commodityDetails = commodityDetails)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(commoditiesDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not get commodity details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
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
