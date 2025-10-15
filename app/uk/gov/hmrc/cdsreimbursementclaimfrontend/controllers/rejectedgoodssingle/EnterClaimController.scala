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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_single_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: ClaimControllerComponents,
  enterClaim: enter_single_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[Claim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val key: String                 = "enter-claim-amount"
  final val postAction: TaxCode => Call = routes.EnterClaimController.submit

  final val showFirst: Action[AnyContent] =
    simpleActionReadClaim(claim =>
      claim.answers.correctedAmounts match {
        case None                   => Redirect(routes.SelectDutiesController.show)
        case Some(correctedAmounts) =>
          val taxCodes: Seq[TaxCode] = claim.getSelectedDuties.getOrElse(Seq.empty)
          correctedAmounts.find((_, claimOpt) => claimOpt.isEmpty) match {
            case None                      => Redirect(routes.CheckClaimDetailsController.show)
            case Some((taxCode, claimOpt)) =>
              Redirect {
                taxCodes.indexOf(taxCode) match {
                  case -1 => // invalid tax code
                    routes.SelectDutiesController.show

                  case n =>
                    routes.EnterClaimController.show(taxCodes(n))
                }
              }
          }
      }
    )

  final def show(taxCode: TaxCode): Action[AnyContent] =
    actionReadClaim { claim =>
      claim.getSelectedDuties match {
        case None =>
          Redirect(routes.SelectDutiesController.show)

        case Some(selectedDuties) if selectedDuties.contains(taxCode) =>
          claim.getNdrcDetailsFor(taxCode) match {
            case None =>
              redirectWhenInvalidTaxCode(claim)

            case Some(ndrcDetails) =>
              val actualAmount: Option[BigDecimal] =
                claim.answers.correctedAmounts.flatMap(_.get(taxCode).flatten)
              val amountPaid                       =
                BigDecimal(ndrcDetails.amount)
              val form                             =
                Forms.claimAmountForm(key, amountPaid).withDefault(actualAmount.map(a => amountPaid - a))
              val maybeMRN                         = claim.getLeadMovementReferenceNumber.map(_.value)
              Ok(
                enterClaim(
                  form,
                  maybeMRN,
                  TaxCode(ndrcDetails.taxType),
                  amountPaid,
                  postAction(taxCode)
                )
              )
          }

        case _ =>
          redirectWhenInvalidTaxCode(claim)
      }
    }

  final def submit(taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteClaim(
      claim =>
        claim.getSelectedDuties match {
          case None =>
            (claim, Redirect(routes.SelectDutiesController.show))

          case Some(selectedDuties) if selectedDuties.contains(taxCode) =>
            val maybeMRN =
              claim.getLeadMovementReferenceNumber.map(_.value)
            claim.getNdrcDetailsFor(taxCode) match {
              case Some(ndrcDetails) =>
                Forms
                  .claimAmountForm(key, BigDecimal(ndrcDetails.amount))
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      (
                        claim,
                        BadRequest(
                          enterClaim(
                            formWithErrors,
                            maybeMRN,
                            TaxCode(ndrcDetails.taxType),
                            BigDecimal(ndrcDetails.amount),
                            postAction(taxCode)
                          )
                        )
                      ),
                    claimAmount =>
                      claim
                        .getNdrcDetailsFor(taxCode) match {
                        case None    => Future.failed(new Exception(s"Cannot find ndrc details for $taxCode"))
                        case Some(_) =>
                          claim
                            .submitClaimAmount(taxCode, claimAmount)
                            .fold(
                              error =>
                                Future.failed(
                                  new Exception(s"Cannot submit amount for $taxCode reimbursement - $error")
                                ),
                              updatedClaim =>
                                (
                                  updatedClaim,
                                  redirectToNextPage(updatedClaim, taxCode)
                                )
                            )
                      }
                  )

              case None =>
                logger.error("Attempting to claim a reimbursement before selecting an MRN")
                (claim, Redirect(routes.EnterMovementReferenceNumberController.show))
            }

          case _ =>
            (claim, redirectWhenInvalidTaxCode(claim))
        },
      fastForwardToCYAEnabled = false
    )

  private def redirectWhenInvalidTaxCode(claim: Claim): Result =
    Redirect {
      if claim.hasCompleteReimbursementClaims then routes.CheckClaimDetailsController.show
      else routes.SelectDutiesController.show
    }

  private def redirectToNextPage(claim: Claim, taxCode: TaxCode): Result =
    if claim.hasCompleteReimbursementClaims && !claim.answers.dutiesChangeMode then
      Redirect(routes.CheckClaimDetailsController.show)
    else {
      val selectedTaxCodes = claim.getSelectedDuties.getOrElse(Seq.empty)
      selectedTaxCodes.indexOf(taxCode) match {
        case -1 => // invalid tax code
          Redirect(routes.SelectDutiesController.show)

        case n if n < selectedTaxCodes.size - 1 =>
          val nextTaxCode = selectedTaxCodes(n + 1)
          claim.answers.correctedAmounts.getOrElse(Map.empty).get(nextTaxCode) match
            case None                                         => Redirect(routes.CheckClaimDetailsController.show)
            case Some(correctAmount) if correctAmount.isEmpty =>
              Redirect(routes.EnterClaimController.show(nextTaxCode))
            case Some(correctAmount)                          => redirectToNextPage(claim, nextTaxCode)

        case _ => Redirect(routes.CheckClaimDetailsController.show)
      }
    }
}
