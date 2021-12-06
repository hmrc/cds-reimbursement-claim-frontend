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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.ChooseClaimTypeController.typeOfClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import scala.concurrent.ExecutionContext

@Singleton
class ChooseClaimTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  chooseClaimTypePage: pages.choose_claim_type
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  def show(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(chooseClaimTypePage(typeOfClaimForm))
    }
}

object ChooseClaimTypeController {
  sealed trait ClaimType
  case object C285 extends ClaimType
  case object CE1179 extends ClaimType

  val dataKey: String = "select-claim-type"

  val typeOfClaimForm: Form[ClaimType] =
    Form(
      mapping(
        dataKey -> text
//          .verifying("invalid", a => TypeOfClaimAnswer.allClaimsTypes.map(_.value).contains(a))
          .transform[ClaimType](
            _ => C285,
            bbb => bbb.toString
          )
      )(identity)(Some(_))
    )
}
