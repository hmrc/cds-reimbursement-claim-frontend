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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.routes as overpaymentsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.routes as overpaymentsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes as overpaymentsSingleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsClaimType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsClaimType.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsClaimType.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature.BasisOfClaimOther
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsClaimType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.choose_how_many_mrns
import uk.gov.hmrc.http.HeaderCarrier
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
  featureSwitchService: FeatureSwitchService,
  chooseHowManyMrnsPage: choose_how_many_mrns
)(implicit
  val ec: ExecutionContext,
  viewConfig: ViewConfig,
  val controllerComponents: MessagesControllerComponents,
  val errorHandler: ErrorHandler
) extends FrontendBaseController
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  private val form: Form[OverpaymentsClaimType] = Forms.overpaymentsChooseHowManyMrnsForm
  private val postAction: Call                  = routes.ChooseHowManyMrnsController.submit

  private def overpaymentsSingleClaimFeatures(implicit
    hc: HeaderCarrier
  ): Option[OverpaymentsSingleClaim.Features] =
    Some(
      OverpaymentsSingleClaim
        .Features(
          shouldAllowOtherBasisOfClaim = featureSwitchService.isEnabled(BasisOfClaimOther)
        )
    )

  private def overpaymentsMultipleClaimFeatures(implicit
    hc: HeaderCarrier
  ): Option[OverpaymentsMultipleClaim.Features] =
    Some(
      OverpaymentsMultipleClaim
        .Features(
          shouldAllowOtherBasisOfClaim = featureSwitchService.isEnabled(BasisOfClaimOther)
        )
    )

  private def overpaymentsScheduledClaimFeatures(implicit
    hc: HeaderCarrier
  ): Option[OverpaymentsScheduledClaim.Features] =
    Some(
      OverpaymentsScheduledClaim
        .Features(
          shouldAllowOtherBasisOfClaim = featureSwitchService.isEnabled(BasisOfClaimOther)
        )
    )

  final val start: Action[AnyContent] =
    Action(Redirect(routes.ChooseHowManyMrnsController.show))

  final val show: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData { implicit request =>
      Ok(chooseHowManyMrnsPage(form, postAction))
    }

  final val submit: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.authenticatedRequest.claimUserType.eoriOpt
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
                  (if request.sessionData.overpaymentsSingleClaim.isEmpty
                     || request.sessionData.overpaymentsSingleClaim.exists(_.isFinalized)
                   then
                     updateSession(sessionStore, request)(
                       SessionData(
                         OverpaymentsSingleClaim.empty(eori, features = overpaymentsSingleClaimFeatures)
                       ).withExistingUserData
                     )
                   else Future.successful(Right(())))
                    .map(_ => Redirect(overpaymentsSingleRoutes.HaveDocumentsReadyController.show))

                case Multiple =>
                  (if request.sessionData.overpaymentsMultipleClaim.isEmpty
                     || request.sessionData.overpaymentsMultipleClaim.exists(_.isFinalized)
                   then
                     updateSession(sessionStore, request)(
                       SessionData(
                         OverpaymentsMultipleClaim.empty(eori, features = overpaymentsMultipleClaimFeatures)
                       ).withExistingUserData
                     )
                   else Future.successful(Right(())))
                    .map(_ => Redirect(overpaymentsMultipleRoutes.HaveDocumentsReadyController.show))

                case Scheduled =>
                  (if request.sessionData.overpaymentsScheduledClaim.isEmpty
                     || request.sessionData.overpaymentsScheduledClaim.exists(_.isFinalized)
                   then
                     updateSession(sessionStore, request)(
                       SessionData(
                         OverpaymentsScheduledClaim.empty(eori, features = overpaymentsScheduledClaimFeatures)
                       ).withExistingUserData
                     )
                   else Future.successful(Right(())))
                    .map(_ => Redirect(overpaymentsScheduledRoutes.HaveDocumentsReadyController.show))
              }
            )
        }
    }

}
