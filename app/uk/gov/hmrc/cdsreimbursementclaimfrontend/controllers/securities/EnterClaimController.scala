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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import cats.syntax.eq.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_claim
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

@Singleton
class EnterClaimController @Inject() (
  val jcc: ClaimControllerComponents,
  enterClaimPage: enter_claim
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  private val key: String = "enter-claim-amount"

  final def showFirst(securityDepositId: String): Action[AnyContent] = simpleActionReadClaim(claim =>
    claim
      .getSelectedDutiesFor(securityDepositId)
      .flatMap(_.headOption) match {

      case Some(firstSelectedDuty) =>
        Redirect(routes.EnterClaimController.show(securityDepositId, firstSelectedDuty))

      case None =>
        Redirect(routes.ConfirmFullRepaymentController.show(securityDepositId))
    }
  )

  final def show(securityDepositId: String, taxCode: TaxCode): Action[AnyContent] =
    actionReadClaim { claim =>
      validateDepositIdAndTaxCode(claim, securityDepositId, taxCode).fold(
        identity,
        { case (correctAmountOpt, paidAmount) =>
          val form = Forms
            .claimAmountForm(key, paidAmount)
            .withDefault(correctAmountOpt.map(a => paidAmount - a))

          Ok(
            enterClaimPage(
              form,
              securityDepositId,
              claim.isSingleSecurity,
              taxCode,
              paidAmount,
              routes.EnterClaimController.submit(securityDepositId, taxCode)
            )
          )
        }
      )
    }

  final def submit(securityDepositId: String, taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteClaim(
      implicit request =>
        claim =>
          validateDepositIdAndTaxCode(claim, securityDepositId, taxCode).fold(
            result => (claim, result),
            { case (_, totalAmount) =>
              val form = Forms.claimAmountForm(key, totalAmount)
              form
                .bindFromRequest()
                .fold(
                  formWithErrors =>
                    (
                      claim,
                      BadRequest(
                        enterClaimPage(
                          formWithErrors,
                          securityDepositId,
                          claim.isSingleSecurity,
                          taxCode,
                          totalAmount,
                          routes.EnterClaimController.submit(securityDepositId, taxCode)
                        )
                      )
                    ),
                  claimAmount => {
                    val amountHasChanged: Boolean =
                      !claim
                        .getClaimAmountFor(securityDepositId, taxCode)
                        .exists(_ === claimAmount)
                    if amountHasChanged then
                      claim
                        .submitClaimAmount(securityDepositId, taxCode, claimAmount)
                        .fold(
                          error =>
                            (
                              claim,
                              Redirect(routeForValidationError(error))
                            ),
                          updatedClaim =>
                            (
                              updatedClaim,
                              Redirect(nextPage(updatedClaim, securityDepositId, taxCode, amountHasChanged = true))
                            )
                        )
                    else (claim, Redirect(nextPage(claim, securityDepositId, taxCode, amountHasChanged = false)))

                  }
                )
            }
          ),
      fastForwardToCYAEnabled = false
    )

  private def validateDepositIdAndTaxCode(claim: SecuritiesClaim, securityDepositId: String, taxCode: TaxCode)(implicit
    request: Request[?]
  ): Either[Result, (Option[BigDecimal], BigDecimal)] = {
    val correctAmountsForDepositId: Option[SecuritiesClaim.CorrectedAmounts] =
      claim.answers.correctedAmounts.flatMap(_.get(securityDepositId))

    correctAmountsForDepositId match {
      case None =>
        if claim.getSecurityDepositIds.contains(securityDepositId) then
          Left(Redirect(routes.ConfirmFullRepaymentController.show(securityDepositId)))
        else
          Left(
            logAndDisplayError(
              s"Invalid depositId=$securityDepositId. Available deposit IDs",
              claim.getSecurityDepositIds.mkString(",")
            )
          )

      case Some(correctAmounts) =>
        correctAmounts.get(taxCode) match {
          case None =>
            Left(Redirect(routes.SelectDutiesController.show(securityDepositId)))

          case Some(correctAmountOpt) =>
            val paidAmountOnDeclaration =
              claim.getSecurityTaxDetailsFor(securityDepositId, taxCode).map(_.getAmount)

            paidAmountOnDeclaration match {
              case None =>
                Left(
                  logAndDisplayError(
                    s"Cannot find the amount of a taxType=$taxCode paid for a depositId=$securityDepositId. Available tax codes",
                    claim.getSecurityTaxCodesFor(securityDepositId).mkString(",")
                  )
                )

              case Some(paidAmount) =>
                Right((correctAmountOpt, paidAmount))
            }

        }
    }
  }

  private def nextPage(
    claim: SecuritiesClaim,
    securityDepositId: String,
    taxCode: TaxCode,
    amountHasChanged: Boolean
  )(using HeaderCarrier): Call =
    if claim.answers.modes.checkClaimDetailsChangeMode && claim.answers.modes.claimFullAmountMode then {
      if claim.userHasSeenCYAPage && !amountHasChanged then routes.CheckYourAnswersController.show
      else
        claim.getNextDepositIdAndTaxCodeToClaim match {
          case Some(Left(depositId)) =>
            routes.ConfirmFullRepaymentController.show(depositId)

          case Some(Right((depositId, tc))) =>
            routes.EnterClaimController.show(depositId, tc)

          case None =>
            if claim.isSingleSecurity then routes.CheckClaimDetailsSingleSecurityController.show
            else routes.CheckClaimDetailsController.show
        }
    } else
      claim
        .getSelectedDutiesFor(securityDepositId)
        .flatMap(_.nextAfter(taxCode)) match {

        case Some(nextTaxCode) =>
          routes.EnterClaimController.show(securityDepositId, nextTaxCode)

        case None =>
          claim.getSelectedDepositIds.nextAfter(securityDepositId) match {
            case Some(nextSecurityDepositId) =>
              if claim.answers.modes.checkClaimDetailsChangeMode && !claim.answers.modes.claimFullAmountMode then
                routes.CheckClaimDetailsController.show
              else routes.ConfirmFullRepaymentController.show(nextSecurityDepositId)

            case None =>
              if claim.isSingleSecurity then routes.CheckClaimDetailsSingleSecurityController.show
              else routes.CheckClaimDetailsController.show
          }
      }

}
