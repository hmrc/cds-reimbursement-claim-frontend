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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claim_details_single

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  checkClaimDetails: check_claim_details_single
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val enterClaimAction: TaxCode => Call = routes.EnterClaimController.show

  final val show: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      (
        claim.withDutiesChangeMode(false),
        if claim.hasCompleteReimbursementClaims
        then
          Ok(
            checkClaimDetails(
              getReimbursementWithCorrectAmount(claim.getReimbursements),
              claim.getSelectedDuties,
              enterClaimAction,
              routes.CheckClaimDetailsController.redirectToSelectDuties,
              routes.CheckClaimDetailsController.continue
            )
          )
        else Redirect(routes.EnterClaimController.showFirst)
      )
    }

  final val redirectToSelectDuties: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      (
        claim.withDutiesChangeMode(true),
        Redirect(routes.SelectDutiesController.show)
      )
    }

  final val continue: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      (
        claim.withDutiesChangeMode(false),
        Redirect(
          if claim.userHasSeenCYAPage then checkYourAnswers
          else routes.EnterInspectionDateController.show
        )
      )
    }

}
