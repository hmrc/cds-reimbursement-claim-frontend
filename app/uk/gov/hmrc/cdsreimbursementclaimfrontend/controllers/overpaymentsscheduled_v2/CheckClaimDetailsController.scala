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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.check_claim_details_scheduled

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetails: check_claim_details_scheduled
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsScheduledJourneyBaseController {

  final val checkClaimDetailsForm: Form[YesNo] = YesOrNoQuestionForm("check-claim-summary")

  implicit val subKey: Option[String] = MRNScheduledRoutes.subKey

  final val selectDutiesAction: Call                      = routes.SelectDutyTypesController.show
  final val enterMrnAction: Call                          = routes.EnterMovementReferenceNumberController.show
  final val enterClaimAction: (DutyType, TaxCode) => Call = routes.EnterClaimController.show
  final val nextAction: Call                              = routes.CheckBankDetailsController.show
  final val postAction: Call                              = routes.CheckClaimDetailsController.submit

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      val answers                        = journey.getReimbursementClaims
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
                reimbursementTotal,
                checkClaimDetailsForm,
                postAction
              )
            }
          case _                                                 =>
            Redirect(selectDutiesAction)
        }
      ).asFuture
    }

  val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      val answers            = journey.getReimbursementClaims
      val reimbursementTotal = journey.getTotalReimbursementAmount

      journey.answers.movementReferenceNumber match {
        case Some(_) =>
          checkClaimDetailsForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  journey,
                  BadRequest(
                    checkClaimDetails(
                      answers,
                      reimbursementTotal,
                      formWithErrors,
                      postAction
                    )
                  )
                ),
              {
                case Yes =>
                  (
                    journey.withDutiesChangeMode(false),
                    Redirect(
                      if (journey.hasCompleteAnswers)
                        checkYourAnswers
                      else
                        routes.CheckBankDetailsController.show
                    )
                  )
                case No  => (journey.withDutiesChangeMode(true), Redirect(selectDutiesAction))
              }
            )
            .asFuture
        case None    =>
          (journey, Redirect(baseRoutes.IneligibleController.ineligible())).asFuture
      }
    },
    fastForwardToCYAEnabled = false
  )
}
