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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimsTableHelper.sortReimbursementsByDisplayDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claim_details_scheduled

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetails: check_claim_details_scheduled
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsScheduledJourneyBaseController {

  implicit val subKey: Option[String] = Some("scheduled")

  final val selectDutiesAction: Call                         = routes.SelectDutyTypesController.show
  final val enterMrnAction: Call                             = routes.EnterMovementReferenceNumberController.show
  final val enterClaimAction: (DutyType, TaxCode) => Call    = routes.EnterClaimController.show
  final val nextAction: Call                                 = routes.EnterBankAccountDetailsController.show
  final val postAction: Call                                 = routes.CheckClaimDetailsController.submit
  final val selectDutyTypesAction: Call                      = routes.SelectDutyTypesController.show
  final val selectDutiesByTypeAction: DutyType => Call       = routes.SelectDutiesController.show
  final val selectExciseDutiesAction: ExciseCategory => Call = routes.SelectDutiesController.showExciseDuties

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      val answers                        = sortReimbursementsByDisplayDuty(journey.getReimbursements)
      val reimbursementTotal: BigDecimal = journey.getTotalReimbursementAmount
      (
        journey.withDutiesChangeMode(false),
        journey.answers.movementReferenceNumber match {
          case None                                              =>
            Redirect(routes.EnterMovementReferenceNumberController.show)
          case Some(_) if journey.hasCompleteReimbursementClaims =>
            Ok {
              checkClaimDetails(
                answers,
                journey.getSelectedDutyTypes.get,
                journey.getNonExciseDutyClaims,
                journey.getSelectedExciseCategoryClaims,
                reimbursementTotal,
                postAction,
                enterClaimAction,
                selectDutyTypesAction,
                selectDutiesByTypeAction,
                selectExciseDutiesAction
              )
            }
          case _                                                 =>
            Redirect(selectDutiesAction)
        }
      ).asFuture
    }

  val submit: Action[AnyContent] = actionReadWriteJourney(implicit request =>
    journey =>
      journey.answers.movementReferenceNumber match {
        case Some(_) =>
          (
            journey.withDutiesChangeMode(false),
            Redirect(
              if journey.hasCompleteAnswers then checkYourAnswers
              else routes.ChoosePayeeTypeController.show
            )
          ).asFuture
        case None    =>
          (journey, Redirect(baseRoutes.IneligibleController.ineligible)).asFuture
      }
  )

}
