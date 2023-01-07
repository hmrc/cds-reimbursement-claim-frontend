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

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.check_claim_details_single
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks._

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetails: check_claim_details_single
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val checkClaimDetailsKey: String = "check-claim.rejected-goods"

  val whetherClaimDetailsCorrect: Form[YesNo] = YesOrNoQuestionForm(checkClaimDetailsKey)

  val enterClaimAction: TaxCode => Call = routes.EnterClaimController.showAmend

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.movementReferenceNumber match {
      case None                                                =>
        Redirect(routes.EnterMovementReferenceNumberController.show()).asFuture
      case Some(mrn) if journey.hasCompleteReimbursementClaims =>
        Ok(
          checkClaimDetails(
            whetherClaimDetailsCorrect,
            mrn,
            journey.getReimbursementClaims.toSeq,
            enterClaimAction,
            routes.CheckClaimDetailsController.submit()
          )
        ).asFuture
      case _                                                   =>
        Redirect(routes.EnterClaimController.show()).asFuture
    }
  }

  val submit: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.movementReferenceNumber match {
      case Some(mrn) =>
        whetherClaimDetailsCorrect
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                checkClaimDetails(
                  formWithErrors,
                  mrn,
                  journey.getReimbursementClaims.toSeq,
                  enterClaimAction,
                  routes.CheckClaimDetailsController.submit()
                )
              ).asFuture,
            {
              case Yes =>
                Redirect(
                  if (journey.userHasSeenCYAPage) checkYourAnswers
                  else routes.EnterInspectionDateController.show()
                ).asFuture
              case No  => Redirect(routes.SelectTaxCodesController.show()).asFuture
            }
          )
      case None      =>
        Redirect(baseRoutes.IneligibleController.ineligible()).asFuture
    }
  }
}
