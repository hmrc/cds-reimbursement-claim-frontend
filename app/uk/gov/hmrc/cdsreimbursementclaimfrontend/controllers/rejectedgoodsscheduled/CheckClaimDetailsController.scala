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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import play.api.mvc.{Action, AnyContent, Call}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DutyType, ExciseCategory, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimsTableHelper.sortReimbursementsByDisplayDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claim_details_scheduled

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  checkClaimDetailsPage: check_claim_details_scheduled
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledClaimBaseController {

  implicit val subKey: Option[String] = Some("scheduled")
  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledClaim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)
  val show: Action[AnyContent] = actionReadWriteClaim { claim =>
    val answers            = sortReimbursementsByDisplayDuty(claim.getReimbursements)
    val reimbursementTotal = claim.getTotalReimbursementAmount
    (
      claim.withDutiesChangeMode(false),
      if claim.hasCompleteReimbursementClaims
      then
        Ok(
          checkClaimDetailsPage(
            answers,
            claim.getSelectedDutyTypes.get,
            claim.getNonExciseDutyClaims,
            claim.getSelectedExciseCategoryClaims,
            reimbursementTotal,
            postAction,
            enterClaimAction,
            selectDutyTypesAction,
            selectDutiesByTypeAction,
            selectExciseDutiesAction
          )
        )
      else Redirect(selectDutiesAction)
    )
  }
  val submit: Action[AnyContent] = actionReadWriteClaim(claim =>
    (
      claim.withDutiesChangeMode(false),
      Redirect(routes.EnterInspectionDateController.show)
    )
  )
  private val postAction: Call                                 = routes.CheckClaimDetailsController.submit
  private val selectDutiesAction: Call                         = routes.SelectDutyTypesController.show
  private val enterClaimAction: (DutyType, TaxCode) => Call    = routes.EnterClaimController.show
  private val selectDutyTypesAction: Call                      = routes.SelectDutyTypesController.show
  private val selectDutiesByTypeAction: DutyType => Call       = routes.SelectDutiesController.show
  private val selectExciseDutiesAction: ExciseCategory => Call = routes.SelectDutiesController.showExciseDuties

}
