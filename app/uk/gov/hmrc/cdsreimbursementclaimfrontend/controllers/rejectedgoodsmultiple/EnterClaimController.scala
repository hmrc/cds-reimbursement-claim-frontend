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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney.Checks._
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
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  val subKey: Option[String] = MRNMultipleRoutes.subKey

  val claimsSummaryAction: Call                 = routes.CheckClaimDetailsController.show()
  val selectDutiesAction: Int => Call           = routes.SelectTaxCodesController.show
  val enterClaimAction: (Int, TaxCode) => Call  = routes.EnterClaimController.show
  val submitClaimAction: (Int, TaxCode) => Call = routes.EnterClaimController.submit

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final def show(pageIndex: Int, taxCode: TaxCode): Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      journey
        .getNthMovementReferenceNumber(pageIndex - 1)
        .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
          journey.getAmountPaidForIfSelected(mrn, taxCode) match {
            case None =>
              logger.warn(s"Claim data for selected MRN and tax code $taxCode does not exist.")
              Redirect(selectDutiesAction(pageIndex))

            case Some(paidAmount) =>
              val claimedAmountOpt = journey.getReimbursementClaimFor(mrn, taxCode)
              val form             = Forms.claimAmountForm(EnterClaimController.key, paidAmount).withDefault(claimedAmountOpt)
              Ok(
                enterClaim(
                  form,
                  taxCode,
                  Some(pageIndex),
                  paidAmount,
                  subKey,
                  submitClaimAction(pageIndex, taxCode)
                )
              )
          }
        }
        .asFuture
    }

  final def submit(pageIndex: Int, taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteJourney(
      { implicit request => journey =>
        journey
          .getNthMovementReferenceNumber(pageIndex - 1)
          .fold((journey, BadRequest(mrnDoesNotExistPage()))) { mrn =>
            journey.getAmountPaidForIfSelected(mrn, taxCode) match {
              case None =>
                // case when tax code not selectable nor selected
                (journey, Redirect(selectDutiesAction(pageIndex)))

              case Some(paidAmount) =>
                Forms
                  .claimAmountForm(EnterClaimController.key, paidAmount)
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      (
                        journey,
                        BadRequest(
                          enterClaim(
                            formWithErrors,
                            taxCode,
                            Some(pageIndex),
                            paidAmount,
                            subKey,
                            submitClaimAction(pageIndex, taxCode)
                          )
                        )
                      ),
                    amount =>
                      journey
                        .submitAmountForReimbursement(mrn, taxCode, amount)
                        .fold(
                          error => {
                            logger.error(s"Error submitting reimbursement claim amount - $error")
                            (journey, Redirect(enterClaimAction(pageIndex, taxCode)))
                          },
                          modifiedJourney =>
                            (modifiedJourney, Redirect(decideNextRoute(modifiedJourney, pageIndex, mrn, taxCode)))
                        )
                  )

            }
          }
          .asFuture
      },
      fastForwardToCYAEnabled = false
    )

  private def decideNextRoute(journey: RejectedGoodsMultipleJourney, pageIndex: Int, mrn: MRN, taxCode: TaxCode): Call =
    if (journey.hasCompleteReimbursementClaims && !journey.answers.dutiesChangeMode)
      claimsSummaryAction
    else {
      val selectedTaxCodes = journey.getSelectedDuties(mrn).getOrElse(Seq.empty)
      selectedTaxCodes.indexOf(taxCode) match {
        case -1 => // invalid tax code
          selectDutiesAction(pageIndex)

        case n if n < selectedTaxCodes.size - 1 =>
          enterClaimAction(pageIndex, selectedTaxCodes(n + 1))

        case _ =>
          journey.getNthMovementReferenceNumber(pageIndex) match {
            case Some(_) => selectDutiesAction(pageIndex + 1)
            case None    => claimsSummaryAction
          }
      }
    }
}

object EnterClaimController {
  val key: String = "enter-claim.rejected-goods"
}
