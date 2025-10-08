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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.mrn_does_not_exist
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_multiple_claims

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterClaimController @Inject() (
  val jcc: ClaimControllerComponents,
  enterMultipleClaims: enter_multiple_claims,
  mrnDoesNotExistPage: mrn_does_not_exist
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val claimsSummaryAction: Call                 = routes.CheckClaimDetailsController.show
  val selectDutiesAction: Int => Call           = routes.SelectDutiesController.show
  val enterClaimAction: (Int, TaxCode) => Call  = routes.EnterClaimController.show
  val submitClaimAction: (Int, TaxCode) => Call = routes.EnterClaimController.submit

  val key: String            = "enter-claim-amount"
  val subKey: Option[String] = Some("multiple")

  final val showFirst: Action[AnyContent] =
    showFirstByIndex(1)

  final def showFirstByIndex(pageIndex: Int): Action[AnyContent] =
    simpleActionReadClaim { claim =>
      Redirect(
        claim
          .getNthMovementReferenceNumber(pageIndex - 1)
          .flatMap(claim.getSelectedDuties(_))
          .flatMap(_.headOption)
          .fold(routes.SelectDutiesController.showFirst)(taxCode =>
            routes.EnterClaimController.show(pageIndex, taxCode)
          )
      )
    }

  final def show(pageIndex: Int, taxCode: TaxCode): Action[AnyContent] =
    actionReadClaim { implicit request => claim =>
      claim
        .getNthMovementReferenceNumber(pageIndex - 1)
        .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
          claim.getAmountPaidForIfSelected(mrn, taxCode) match {
            case None =>
              logger.warn(s"Claim data for selected MRN and tax code $taxCode does not exist.")
              Redirect(selectDutiesAction(pageIndex))

            case Some(amountPaid) =>
              val actualAmount =
                claim.getCorrectedAmountFor(mrn, taxCode)

              val form =
                Forms
                  .claimAmountForm(key, amountPaid)
                  .withDefault(actualAmount.map(a => amountPaid - a))

              Ok(
                enterMultipleClaims(
                  form,
                  pageIndex,
                  mrn,
                  taxCode,
                  amountPaid,
                  submitClaimAction(pageIndex, taxCode)
                )
              )
          }
        }

    }

  final def submit(pageIndex: Int, taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteClaim(
      implicit request =>
        claim =>
          claim
            .getNthMovementReferenceNumber(pageIndex - 1)
            .fold((claim, BadRequest(mrnDoesNotExistPage()))) { mrn =>
              claim.getAmountPaidForIfSelected(mrn, taxCode) match {
                case None =>
                  // case when tax code not selectable nor selected
                  (claim, Redirect(selectDutiesAction(pageIndex)))

                case Some(amountPaid) =>
                  Forms
                    .claimAmountForm(key, amountPaid)
                    .bindFromRequest()
                    .fold(
                      formWithErrors =>
                        (
                          claim,
                          BadRequest(
                            enterMultipleClaims(
                              formWithErrors,
                              pageIndex,
                              mrn,
                              taxCode,
                              amountPaid,
                              submitClaimAction(pageIndex, taxCode)
                            )
                          )
                        ),
                      amount =>
                        claim
                          .submitClaimAmount(mrn, taxCode, amount)
                          .fold(
                            error => {
                              logger.error(s"Error submitting reimbursement claim amount - $error")
                              (claim, Redirect(enterClaimAction(pageIndex, taxCode)))
                            },
                            modifiedClaim =>
                              (modifiedClaim, Redirect(decideNextRoute(modifiedClaim, pageIndex, mrn, taxCode)))
                          )
                    )

              }
            },
      fastForwardToCYAEnabled = false
    )

  def decideNextRoute(claim: RejectedGoodsMultipleClaim, pageIndex: Int, mrn: MRN, taxCode: TaxCode): Call =
    if claim.hasCompleteReimbursementClaims && !claim.answers.dutiesChangeMode then claimsSummaryAction
    else {
      val selectedTaxCodes = claim.getSelectedDuties(mrn).getOrElse(Seq.empty)
      selectedTaxCodes.indexOf(taxCode) match {
        case -1 => // invalid tax code
          selectDutiesAction(pageIndex)

        case n if n < selectedTaxCodes.size - 1 =>
          enterClaimAction(pageIndex, selectedTaxCodes(n + 1))

        case _ =>
          claim.getNthMovementReferenceNumber(pageIndex) match {
            case Some(_) => selectDutiesAction(pageIndex + 1)
            case None    => claimsSummaryAction
          }
      }
    }

}
