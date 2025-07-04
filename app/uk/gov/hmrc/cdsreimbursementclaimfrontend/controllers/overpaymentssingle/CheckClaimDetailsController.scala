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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claim_details_single

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetails: check_claim_details_single
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val enterClaimAction: TaxCode => Call = routes.EnterClaimController.show

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      (
        journey.withDutiesChangeMode(false),
        journey.answers.movementReferenceNumber match {
          case None                                                =>
            Redirect(routes.EnterMovementReferenceNumberController.show)
          case Some(mrn) if journey.hasCompleteReimbursementClaims =>
            Ok(
              checkClaimDetails(
                getReimbursementWithCorrectAmount(journey.getReimbursements),
                journey.getSelectedDuties,
                enterClaimAction,
                routes.CheckClaimDetailsController.redirectToSelectDuties,
                routes.CheckClaimDetailsController.continue
              )
            )

          case _ =>
            Redirect(routes.EnterClaimController.showFirst)
        }
      ).asFuture
    }

  final val redirectToSelectDuties: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      (
        journey.withDutiesChangeMode(true),
        Redirect(routes.SelectDutiesController.show)
      ).asFuture
    }

  final val continue: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      (
        journey.withDutiesChangeMode(false),
        Redirect(
          if journey.userHasSeenCYAPage then checkYourAnswers
          else routes.ChoosePayeeTypeController.show
        )
      ).asFuture
    }

}
