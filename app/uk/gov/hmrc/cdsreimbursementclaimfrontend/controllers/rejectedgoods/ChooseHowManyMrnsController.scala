/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.chooseHowManyMrnsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.{routes => rejectedGoodsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.{routes => rejectedGoodsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.{routes => rejectedGoodsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseHowManyMrnsController @Inject() (
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val sessionStore: SessionCache,
  chooseHowManyMrnsPage: pages.choose_how_many_mrns
)(implicit
  val ec: ExecutionContext,
  viewConfig: ViewConfig,
  val controllerComponents: MessagesControllerComponents,
  val errorHandler: ErrorHandler
) extends FrontendBaseController
    with SessionDataExtractor
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  val dataKey: String                      = "rejected-goods.choose-how-many-mrns"
  val form: Form[RejectedGoodsJourneyType] = chooseHowManyMrnsForm
  private val postAction: Call             = routes.ChooseHowManyMrnsController.submit()

  final val start: Action[AnyContent] =
    Action(Redirect(routes.ChooseHowManyMrnsController.show()))

  final val show: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData { implicit request =>
      Ok(chooseHowManyMrnsPage(form, postAction))
    }

  final val submit: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.authenticatedRequest.journeyUserType.eoriOpt
        .fold[Future[Result]](Future.failed(new Exception("User is missing EORI number"))) { eori =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Future
                  .successful(
                    BadRequest(chooseHowManyMrnsPage(formWithErrors, postAction))
                  ),
              {
                case Individual =>
                  (if (request.sessionData.rejectedGoodsSingleJourney.isEmpty)
                     updateSession(sessionStore, request)(
                       SessionData(RejectedGoodsSingleJourney.empty(eori)).withExistingUserData
                     )
                   else
                     Future.successful(Right(())))
                    .map(_ => Redirect(rejectedGoodsSingleRoutes.EnterMovementReferenceNumberController.show()))

                case Multiple =>
                  (if (request.sessionData.rejectedGoodsMultipleJourney.isEmpty)
                     updateSession(sessionStore, request)(
                       SessionData(RejectedGoodsMultipleJourney.empty(eori)).withExistingUserData
                     )
                   else
                     Future.successful(Right(())))
                    .map(_ => Redirect(rejectedGoodsMultipleRoutes.EnterMovementReferenceNumberController.showFirst()))

                case Scheduled =>
                  (if (request.sessionData.rejectedGoodsScheduledJourney.isEmpty)
                     updateSession(sessionStore, request)(
                       SessionData(RejectedGoodsScheduledJourney.empty(eori)).withExistingUserData
                     )
                   else
                     Future.successful(Right(())))
                    .map(_ => Redirect(rejectedGoodsScheduledRoutes.EnterMovementReferenceNumberController.show()))

              }
            )
        }
    }

}
