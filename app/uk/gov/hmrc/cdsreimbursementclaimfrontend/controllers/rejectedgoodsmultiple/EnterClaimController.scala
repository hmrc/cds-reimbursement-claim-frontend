/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.mrn_does_not_exist
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaim: enter_claim,
  mrnDoesNotExistPage: mrn_does_not_exist
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  val key: String          = "enter-claim.rejected-goods"
  val subKey: Some[String] = Some("multiple")

  val claimsSummaryAction: Call                 = routes.CheckClaimDetailsController.show()
  val selectDutiesAction: Int => Call           = routes.SelectTaxCodesController.show(_)
  val enterClaimAction: (Int, TaxCode) => Call  = routes.EnterClaimController.show(_, _)
  val submitClaimAction: (Int, TaxCode) => Call = routes.EnterClaimController.submit(_, _)

  def show(index: Int, taxCode: TaxCode): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey
      .getNthMovementReferenceNumber(index - 1)
      .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
        journey.getAmountPaidForIfSelected(mrn, taxCode) match {
          case None =>
            Redirect(selectDutiesAction(index))

          case Some(paidAmount) =>
            val claimedAmountOpt = journey.getReimbursementClaimFor(mrn, taxCode)
            val form             = Forms.claimAmountForm(key, paidAmount).withDefault(claimedAmountOpt)
            Ok(
              enterClaim(
                form,
                taxCode,
                Some(index),
                paidAmount,
                subKey,
                submitClaimAction(index, taxCode)
              )
            )
        }
      }
      .asFuture
  }

  def submit(index: Int, taxCode: TaxCode): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      journey
        .getNthMovementReferenceNumber(index - 1)
        .fold((journey, BadRequest(mrnDoesNotExistPage()))) { mrn =>
          journey.getAmountPaidForIfSelected(mrn, taxCode) match {
            case None =>
              // case when tax code not selectable nor selected
              (journey, Redirect(selectDutiesAction(index)))

            case Some(paidAmount) =>
              Forms
                .claimAmountForm(key, paidAmount)
                .bindFromRequest()
                .fold(
                  formWithErrors =>
                    (
                      journey,
                      BadRequest(
                        enterClaim(
                          formWithErrors,
                          taxCode,
                          Some(index),
                          paidAmount,
                          subKey,
                          submitClaimAction(index, taxCode)
                        )
                      )
                    ),
                  amount =>
                    journey
                      .submitAmountForReimbursement(mrn, taxCode, amount)
                      .fold(
                        error => {
                          logger.error(s"Error submitting reimbursement claim amount - $error")
                          (journey, Redirect(enterClaimAction(index, taxCode)))
                        },
                        modifiedJourney =>
                          (modifiedJourney, Redirect(decideNextRoute(modifiedJourney, index, mrn, taxCode)))
                      )
                )

          }
        }
        .asFuture
    },
    fastForwardToCYAEnabled = false
  )

  def decideNextRoute(journey: RejectedGoodsMultipleJourney, index: Int, mrn: MRN, taxCode: TaxCode): Call =
    if (journey.hasCompleteReimbursementClaims && !journey.answers.dutiesChangeMode)
      claimsSummaryAction
    else {
      val selectedTaxCodes = journey.getSelectedDuties(mrn).getOrElse(Seq.empty)
      selectedTaxCodes.indexOf(taxCode) match {
        case -1 => // invalid tax code
          selectDutiesAction(index)

        case n if n < selectedTaxCodes.size - 1 =>
          enterClaimAction(index, selectedTaxCodes(n + 1))

        case _ =>
          journey.getNthMovementReferenceNumber(index) match {
            case Some(_) => selectDutiesAction(index + 1)
            case None    => claimsSummaryAction
          }
      }
    }
}
