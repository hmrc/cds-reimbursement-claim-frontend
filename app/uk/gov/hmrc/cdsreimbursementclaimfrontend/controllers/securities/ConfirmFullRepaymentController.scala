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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment

import scala.concurrent.ExecutionContext

@Singleton
class ConfirmFullRepaymentController @Inject() (
  val jcc: ClaimControllerComponents,
  confirmFullRepaymentPage: confirm_full_repayment
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {

  private val form: Form[YesNo] = confirmFullRepaymentForm

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val showFirst: Action[AnyContent] =
    actionReadClaim { claim =>
      claim.getSelectedDepositIds.headOption
        .fold(
          Redirect(routes.CheckDeclarationDetailsController.show)
        )(id => Redirect(routes.ConfirmFullRepaymentController.show(id)))
    }

  private def getPageModel(displayDeclaration: DisplayDeclaration, id: String): ConfirmFullRepaymentModel =
    ConfirmFullRepaymentModel(
      mrn = displayDeclaration.getMRN.value,
      securityId = id,
      depositValue = displayDeclaration.getSecurityTotalValueFor(id).toPoundSterlingString
    )

  def show(id: String): Action[AnyContent] = actionReadWriteClaim { claim =>
    claim
      .getDisplayDeclarationIfValidSecurityDepositId(id)
      .map(getPageModel(_, id))
      .fold((claim, errorHandler.errorResult())) { case model =>
        (
          claim.resetClaimFullAmountMode(),
          Ok(
            confirmFullRepaymentPage(
              form.withDefault(claim.getClaimFullAmountStatus(id)),
              model,
              routes.ConfirmFullRepaymentController.submit(id)
            )
          )
        )
      }

  }

  def submit(id: String): Action[AnyContent] = actionReadWriteClaim(
    claim =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              claim
                .getDisplayDeclarationIfValidSecurityDepositId(id)
                .map(getPageModel(_, id))
                .map { case model =>
                  BadRequest(
                    confirmFullRepaymentPage(
                      formWithErrors,
                      model,
                      routes.ConfirmFullRepaymentController.submit(id)
                    )
                  )
                }
                .getOrElse(errorHandler.errorResult())
            ),
          answer =>
            if claim.getClaimFullAmountStatus(id).contains(answer) &&
              claim.userHasSeenCYAPage
            then (claim, Redirect(checkYourAnswers))
            else
              answer match {
                case Yes =>
                  submitYes(id, claim)
                case No  =>
                  submitNo(id, claim)
              }
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
        { updatedClaim =>
          val nextRoute =
            if claim.answers.modes.checkClaimDetailsChangeMode then routes.CheckClaimDetailsController.show
            else
              claim.getSelectedDepositIds
                .nextAfter(securityId)
                .fold(routes.CheckClaimDetailsController.show) { nextSecurityId =>
                  routes.ConfirmFullRepaymentController.show(nextSecurityId)
                }
          (updatedClaim, Redirect(nextRoute))
        }
      )

  def submitNo(securityId: String, claim: SecuritiesClaim): (SecuritiesClaim, Result) =
    if claim.getSelectedDutiesFor(securityId).isEmpty || claim.isFullSecurityAmountClaimed(securityId) then {
      if claim.getSecurityTaxCodesFor(securityId).size == 1 then
        claim
          .submitClaimFullAmountMode(false)
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(
            securityId,
            claim.getSecurityTaxCodesFor(securityId)
          )
          .fold(
            error => {
              logger.warn(error)
              (claim, Redirect(baseRoutes.IneligibleController.ineligible))
            },
            updatedClaim =>
              (
                updatedClaim,
                Redirect(routes.EnterClaimController.showFirst(securityId))
              )
          )
      else
        (
          claim.submitClaimFullAmountMode(false),
          Redirect(routes.SelectDutiesController.show(securityId))
        )
    } else
      (
        claim,
        Redirect(routes.CheckClaimDetailsController.show)
      )
}

final case class ConfirmFullRepaymentModel(mrn: String, securityId: String, depositValue: String)
