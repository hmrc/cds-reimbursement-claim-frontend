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

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, text}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.ChooseClaimTypeController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.{routes => rejectGoodsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ChooseClaimTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  chooseClaimTypePage: pages.choose_claim_type
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  def show(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(chooseClaimTypePage(claimFormForm))
    }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    claimFormForm
      .bindFromRequest()
      .fold(
        formWithErrors => {
          if (formWithErrors.data.nonEmpty)
            logger.error(s"Invalid claim form type supplied - ${formWithErrors.data.values.mkString}")
          Future.successful(BadRequest(chooseClaimTypePage(formWithErrors)))
        },
        {
          case C285          => Future.successful(Redirect(claimRoutes.SelectTypeOfClaimController.show()))
          case RejectedGoods => rejectedGoods(sessionStore, request)
        }
      )
  }

  private def rejectedGoods(
    sessionStore: SessionCache,
    request: RequestWithSessionData[_]
  )(implicit hc: HeaderCarrier): Future[Result] =
    (request.sessionData, request.signedInUserDetails) match {
      case (Some(sessionData), Some(user)) if sessionData.rejectedGoodsSingleJourney.isEmpty =>
        updateSession(sessionStore, request)(
          _.copy(rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(user.eori)))
        ).map { _ =>
          Redirect(rejectGoodsRoutes.EnterMovementReferenceNumberController.show())
        }
      case _                                                                                 =>
        Future.successful(Redirect(rejectGoodsRoutes.EnterMovementReferenceNumberController.show()))
    }
}

object ChooseClaimTypeController {
  sealed trait ClaimForm
  case object C285 extends ClaimForm
  case object RejectedGoods extends ClaimForm

  val allowedValues: Seq[String] = Seq("C285", "RejectedGoods")

  val dataKey: String = "choose-claim-type"

  val claimFormForm: Form[ClaimForm] =
    Form(
      mapping(
        dataKey -> text
          .verifying(value => allowedValues.contains(value))
          .transform[ClaimForm](
            {
              case "RejectedGoods" => RejectedGoods
              case "C285"          => C285
            },
            _.toString
          )
      )(identity)(Some(_))
    )
}
