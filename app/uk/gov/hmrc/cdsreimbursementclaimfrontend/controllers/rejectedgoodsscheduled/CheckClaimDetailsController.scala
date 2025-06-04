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

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimsTableHelper.sortReimbursementsByDisplayDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claim_details_scheduled

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetailsPage: check_claim_details_scheduled
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  implicit val subKey: Option[String] = Some("scheduled")

  private val postAction: Call                              = routes.CheckClaimDetailsController.submit
  private val selectDutiesAction: Call                      = routes.SelectDutyTypesController.show
  private val enterClaimAction: (DutyType, TaxCode) => Call = routes.EnterClaimController.show

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val answers            = sortReimbursementsByDisplayDuty(journey.getReimbursements)
    val reimbursementTotal = journey.getTotalReimbursementAmount
    (
      journey.withDutiesChangeMode(false),
      if !journey.hasCompleteReimbursementClaims then Redirect(selectDutiesAction)
      else {
        Ok(
          checkClaimDetailsPage(
            answers,
            journey.getSelectedDutyTypes.get,
            journey.getNonExciseDutyClaims,
            journey.getSelectedExciseCategoryClaims,
            reimbursementTotal,
            postAction,
            enterClaimAction
          )
        )
      }
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney(implicit request =>
    journey =>
      if !journey.hasCompleteReimbursementClaims then (journey, Redirect(selectDutiesAction)).asFuture
      else {
        (
          journey.withDutiesChangeMode(false),
          Redirect(
            if journey.hasCompleteAnswers then checkYourAnswers
            else routes.EnterInspectionDateController.show
          )
        ).asFuture
      }
  )
}
