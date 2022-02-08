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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import play.api.data.Form

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.chooseHowManyMrnsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.{routes => rejectedGoodsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.{routes => rejectedGoodsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseHowManyMrnsController @Inject() (
  val jcc: JourneyControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  chooseHowManyMrnsPage: pages.choose_how_many_mrns
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with SessionDataExtractor
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  val formKey: String                      = "rejected-goods.choose-how-many-mrns"
  val form: Form[RejectedGoodsJourneyType] = chooseHowManyMrnsForm
  private val postAction: Call             = routes.ChooseHowManyMrnsController.submit()

  val show: Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(chooseHowManyMrnsPage(form, RejectedGoodsJourneyType.values, postAction))
  }

  val submit: Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Future
            .successful(BadRequest(chooseHowManyMrnsPage(formWithErrors, RejectedGoodsJourneyType.values, postAction))),
        {
          case Individual =>
            val individualRoute: Call = rejectedGoodsSingleRoutes.EnterMovementReferenceNumberController.show()
            rejectedGoods(sessionStore, request, individualRoute)
          case Multiple   =>
            Future.successful(Redirect("/rejected-goods/multiple/enter-movement-reference-number"))
          case _          =>
            Future.successful(Redirect("/rejected-goods/scheduled/enter-movement-reference-number"))

        }
      )
  }

  private def rejectedGoods(
    sessionStore: SessionCache,
    request: RequestWithSessionData[_],
    route: Call
  )(implicit hc: HeaderCarrier): Future[Result] =
    (request.sessionData, request.signedInUserDetails) match {
      case (Some(sessionData), Some(user)) if sessionData.rejectedGoodsSingleJourney.isDefined   =>
        logger.error("&&&&&&&&&&&&& ********  Individual")
        updateSession(sessionStore, request)(
          _.copy(rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(user.eori)))
        ).map(_ => Redirect(route))
      case (Some(sessionData), Some(user)) if sessionData.rejectedGoodsMultipleJourney.isDefined =>
        logger.error("&&&&&&&&&&&&& ********  Multiple")
        updateSession(sessionStore, request)(
          _.copy(rejectedGoodsMultipleJourney = Some(RejectedGoodsMultipleJourney.empty(user.eori)))
        ).map(_ => Redirect("/rejected-goods/multiple/enter-movement-reference-number"))
      case _                                                                                     =>
        Future.successful(Redirect("/rejected-goods/scheduled/enter-movement-reference-number"))
    }
}
