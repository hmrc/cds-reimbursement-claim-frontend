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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.confirmFullRepaymentForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndImportDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment_for_single_depositId

import scala.concurrent.ExecutionContext

@Singleton
class ConfirmSingleDepositRepaymentController @Inject() (
  val jcc: ClaimControllerComponents,
  confirmFullRepaymentPageForSingleDepositId: confirm_full_repayment_for_single_depositId
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {

  private val form: Form[YesNo] = confirmFullRepaymentForm

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndImportDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val show: Action[AnyContent] = actionReadWriteClaim { claim =>
    claim.getSecurityDetails.headOption
      .fold((claim, errorHandler.errorResult())) { case securityDetail =>
        (
          claim.resetClaimFullAmountMode(),
          Ok(
            confirmFullRepaymentPageForSingleDepositId(
              form.withDefault(claim.getClaimFullAmountStatus(securityDetail.securityDepositId)),
              securityDetail,
              routes.ConfirmSingleDepositRepaymentController.submit
            )
          )
        )
      }

  }

  val submit: Action[AnyContent] = actionReadWriteClaim(
    claim =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              claim.getSecurityDetails.headOption
                .map { case securityDetail =>
                  BadRequest(
                    confirmFullRepaymentPageForSingleDepositId(
                      formWithErrors,
                      securityDetail,
                      routes.ConfirmSingleDepositRepaymentController.submit
                    )
                  )
                }
                .getOrElse(errorHandler.errorResult())
            ),
          answer =>
            claim.getSecurityDetails.headOption
              .map { case securityDetail =>
                if claim.getClaimFullAmountStatus(securityDetail.securityDepositId).contains(answer) &&
                  claim.userHasSeenCYAPage
                then (claim, Redirect(checkYourAnswers))
                else
                  answer match {
                    case Yes =>
                      submitYes(securityDetail.securityDepositId, claim)
                    case No  =>
                      submitNo(securityDetail.securityDepositId, claim)
                  }

              }
              .getOrElse((claim, errorHandler.errorResult()))
        ),
    fastForwardToCYAEnabled = false
  )

  def submitYes(securityId: String, claim: SecuritiesClaim)(implicit
    request: Request[?]
  ): (SecuritiesClaim, Result) =
    claim
      .submitFullCorrectedAmounts(securityId)
      .fold(
        { error =>
          logger.warn(error)
          (claim, errorHandler.errorResult())
        },
        updatedClaim =>
          (
            updatedClaim,
            Redirect {
              if claim.userHasSeenCYAPage then routes.CheckYourAnswersController.show
              else if claim.getReasonForSecurity.exists(ntas.contains) then routes.ChooseExportMethodController.show
              else routes.ChoosePayeeTypeController.show
            }
          )
      )

  def submitNo(securityId: String, claim: SecuritiesClaim)(implicit
    request: Request[?]
  ): (SecuritiesClaim, Result) =
    claim
      .clearCorrectedAmounts(securityId)
      .fold(
        { error =>
          logger.warn(error)
          (claim, errorHandler.errorResult())
        },
        updatedClaim => (updatedClaim, Redirect(routes.PartialClaimsController.show))
      )

}
