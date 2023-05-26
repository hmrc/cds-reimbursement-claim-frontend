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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2.{routes => overpaymentsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.choose_how_many_mrns
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

@Singleton
class ChooseHowManyMrnsController @Inject() (
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val sessionStore: SessionCache,
  featureSwitchService: FeatureSwitchService,
  chooseHowManyMrnsPage: choose_how_many_mrns
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

  private val form: Form[OverpaymentsJourneyType] = Forms.overpaymentsChooseHowManyMrnsForm
  private val postAction: Call                    = routes.ChooseHowManyMrnsController.submit()

  private def overpaymentsSingleJourneyFeatures(implicit
    hc: HeaderCarrier
  ): Option[OverpaymentsSingleJourney.Features] =
    featureSwitchService.optionally(
      Feature.BlockSubsidies,
      OverpaymentsSingleJourney.Features(shouldBlockSubsidies = true)
    )

  private def overpaymentsMultipleJourneyFeatures(implicit
    hc: HeaderCarrier
  ): Option[OverpaymentsMultipleJourney.Features] =
    featureSwitchService.optionally(
      Feature.BlockSubsidies,
      OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = true)
    )

  private def overpaymentsScheduledJourneyFeatures(implicit
    hc: HeaderCarrier
  ): Option[OverpaymentsScheduledJourney.Features] =
    featureSwitchService.optionally(
      Feature.BlockSubsidies,
      OverpaymentsScheduledJourney.Features(shouldBlockSubsidies = true)
    )

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
                  (if (request.sessionData.overpaymentsSingleJourney.isEmpty)
                     updateSession(sessionStore, request)(
                       SessionData(
                         OverpaymentsSingleJourney.empty(eori, features = overpaymentsSingleJourneyFeatures)
                       ).withExistingUserData
                     )
                   else
                     Future.successful(Right(())))
                    .map(_ => Redirect(overpaymentsSingleRoutes.EnterMovementReferenceNumberController.show))

                case Multiple =>
                  (if (request.sessionData.overpaymentsMultipleJourney.isEmpty)
                     updateSession(sessionStore, request)(
                       SessionData(
                         OverpaymentsMultipleJourney.empty(eori, features = overpaymentsMultipleJourneyFeatures)
                       ).withExistingUserData
                     )
                   else
                     Future.successful(Right(())))
                    .map(_ => Redirect(overpaymentsMultipleRoutes.EnterMovementReferenceNumberController.showFirst))

                case Scheduled =>
                  (if (request.sessionData.overpaymentsScheduledJourney.isEmpty)
                     updateSession(sessionStore, request)(
                       SessionData(
                         OverpaymentsScheduledJourney.empty(eori, features = overpaymentsScheduledJourneyFeatures)
                       ).withExistingUserData
                     )
                   else
                     Future.successful(Right(())))
                    .map(_ => Redirect(overpaymentsScheduledRoutes.EnterMovementReferenceNumberController.show))
              }
            )
        }
    }

}
