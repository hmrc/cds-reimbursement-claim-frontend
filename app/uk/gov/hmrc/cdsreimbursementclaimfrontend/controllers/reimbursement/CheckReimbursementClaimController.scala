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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.CheckReimbursementClaimController.whetherDutiesCorrectForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class CheckReimbursementClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  checkReimbursementClaim: pages.check_reimbursement_claim
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[ReimbursementClaimAnswer] = _.reimbursementClaimAnswer

  def showReimbursementClaim(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[ReimbursementClaimAnswer] { (fillingOutClaim, maybeReimbursementClaimAnswer) =>
      implicit val routes: ReimbursementRoutes =
        extractRoutes(fillingOutClaim.draftClaim, JourneyBindable.Scheduled)

      maybeReimbursementClaimAnswer.fold(
        Redirect(reimbursementRoutes.SelectDutyCodesController.start())
      )(answer => Ok(checkReimbursementClaim(whetherDutiesCorrectForm, answer)))
    }
  }

  def submitReimbursementClaim(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[ReimbursementClaimAnswer] { (fillingOutClaim, maybeReimbursementClaimAnswer) =>
      implicit val routes: ReimbursementRoutes =
        extractRoutes(fillingOutClaim.draftClaim, JourneyBindable.Scheduled)

      maybeReimbursementClaimAnswer.fold(
        Redirect(reimbursementRoutes.SelectDutyCodesController.start())
      )(answer =>
        whetherDutiesCorrectForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(checkReimbursementClaim(formWithErrors, answer)),
            {
              case Yes =>
                Redirect(claimsRoutes.BankAccountController.checkBankAccountDetails(JourneyBindable.Scheduled))
              case No  =>
                Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes())
            }
          )
      )
    }
  }
}

object CheckReimbursementClaimController {

  val whetherDutiesCorrectForm: Form[YesNo] = Form(
    mapping(
      "check-claim-summary" -> optional(boolean)
        .verifying("error.invalid", _.isDefined)
        .transform[YesNo](
          value => if (value.exists(_ === true)) Yes else No,
          answer => Some(answer === Yes)
        )
    )(identity)(Some(_))
  )
}
