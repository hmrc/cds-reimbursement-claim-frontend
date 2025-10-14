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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.text
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.ChooseClaimTypeController.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments.routes as overpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods.routes as rejectGoodsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.ReasonForSecurityHelper
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.routes as securitiesRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_claim_type
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseClaimTypeController @Inject() (
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitchService: FeatureSwitchService,
  chooseClaimTypePage: choose_claim_type
)(implicit viewConfig: ViewConfig, val controllerComponents: MessagesControllerComponents, ec: ExecutionContext)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  final val show: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData { implicit request =>
      Ok(chooseClaimTypePage(claimFormForm))
    }

  final val submit: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      claimFormForm
        .bindFromRequest()
        .fold(
          formWithErrors => {
            if formWithErrors.data.nonEmpty then
              logger.error(s"Invalid claim form type supplied - ${formWithErrors.data.values.mkString}")
            Future.successful(BadRequest(chooseClaimTypePage(formWithErrors)))
          },
          {
            case C285 =>
              Future.successful(Redirect(overpaymentsRoutes.ChooseHowManyMrnsController.show))

            case RejectedGoods =>
              Future.successful(Redirect(rejectGoodsRoutes.ChooseHowManyMrnsController.show))

            case Securities =>
              val reasonForSecurityHelper = new ReasonForSecurityHelper(
                configuration = viewConfig.config
              )
              request.authenticatedRequest.claimUserType.eoriOpt
                .fold[Future[Result]](Future.failed(new Exception("User is missing EORI number"))) { eori =>
                  sessionStore
                    .store(
                      SessionData(
                        SecuritiesClaim.empty(
                          eori,
                          Nonce.random,
                          features = Some(
                            SecuritiesClaim.Features(availableReasonsForSecurity =
                              reasonForSecurityHelper.availableReasonsForSecurity()
                            )
                          )
                        )
                      )
                        .withExistingUserData(request.sessionData)
                    )
                    .map(_ => Redirect(securitiesRoutes.EnterMovementReferenceNumberController.show))
                }
          }
        )
    }

}

object ChooseClaimTypeController {
  sealed trait ClaimForm
  case object C285 extends ClaimForm
  case object RejectedGoods extends ClaimForm
  case object Securities extends ClaimForm

  val allowedValues: Seq[String] = Seq("C285", "RejectedGoods", "Securities")

  val dataKey: String = "choose-claim-type"

  val claimFormForm: Form[ClaimForm] =
    Form(
      mapping(
        dataKey -> text
          .verifying(value => allowedValues.contains(value))
          .transform[ClaimForm](
            {
              case "C285"          => C285
              case "RejectedGoods" => RejectedGoods
              case "Securities"    => Securities
            },
            _.toString
          )
      )(identity)(Some(_))
    )
}
