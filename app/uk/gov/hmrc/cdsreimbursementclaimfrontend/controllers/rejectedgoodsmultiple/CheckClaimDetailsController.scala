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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.check_claim_details_multiple

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetails: check_claim_details_multiple
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  val form: Form[YesNo] = YesOrNoQuestionForm("check-claim.rejected-goods")

  val submitAction: Call                       = routes.CheckClaimDetailsController.submit()
  val selectDutiesAction: Call                 = routes.SelectDutiesController.showFirst
  val enterMrnAction: Call                     = routes.EnterMovementReferenceNumberController.showFirst()
  val enterClaimAction: (Int, TaxCode) => Call = routes.EnterClaimController.show
  val nextAction: Call                         = routes.EnterInspectionDateController.show()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val show: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    (
      journey.withDutiesChangeMode(false),
      if (!journey.hasCompleteMovementReferenceNumbers) Redirect(enterMrnAction)
      else if (!journey.hasCompleteReimbursementClaims) Redirect(selectDutiesAction)
      else {
        Ok(
          checkClaimDetails(
            form,
            getClaimsForDisplay(journey),
            enterClaimAction,
            submitAction
          )
        )
      }
    ).asFuture
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      (if (!journey.hasCompleteMovementReferenceNumbers) (journey, Redirect(enterMrnAction))
       else if (!journey.hasCompleteReimbursementClaims) (journey, Redirect(selectDutiesAction))
       else {
         form
           .bindFromRequest()
           .fold(
             formWithErrors =>
               (
                 journey,
                 Ok(
                   checkClaimDetails(
                     formWithErrors,
                     getClaimsForDisplay(journey),
                     enterClaimAction,
                     submitAction
                   )
                 )
               ),
             {
               case Yes =>
                 (
                   journey,
                   if (shouldForwardToCYA(journey)) Redirect(checkYourAnswers)
                   else Redirect(nextAction)
                 )
               case No  => (journey.withDutiesChangeMode(true), Redirect(selectDutiesAction))
             }
           )
       }).asFuture
    },
    fastForwardToCYAEnabled = false
  )

  private def getClaimsForDisplay(journey: RejectedGoodsMultipleJourney): Seq[(MRN, Int, Map[TaxCode, BigDecimal])] =
    journey.getReimbursementClaims.toSeq.zipWithIndex
      .map { case ((mrn, claims), index) => (mrn, index + 1, claims) }
}
