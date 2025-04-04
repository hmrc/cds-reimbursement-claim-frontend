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

import com.github.arturopala.validator.Validator.Validate
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment_for_single_depositId

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ConfirmSingleDepositRepaymentController @Inject() (
  val jcc: JourneyControllerComponents,
  confirmFullRepaymentPageForSingleDepositId: confirm_full_repayment_for_single_depositId
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val form: Form[YesNo] = confirmFullRepaymentForm

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    journey.getSecurityDetails.headOption
      .fold((journey, errorHandler.errorResult())) { case securityDetail =>
        (
          journey.resetClaimFullAmountMode(),
          Ok(
            confirmFullRepaymentPageForSingleDepositId(
              form.withDefault(journey.getClaimFullAmountStatus(securityDetail.securityDepositId)),
              securityDetail,
              routes.ConfirmSingleDepositRepaymentController.submit
            )
          )
        )
      }
      .asFuture
  }

  def submit: Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      journey =>
        form
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                journey.getSecurityDetails.headOption
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
              ).asFuture,
            answer =>
              journey.getSecurityDetails.headOption
                .map { case securityDetail =>
                  if journey.getClaimFullAmountStatus(securityDetail.securityDepositId).contains(answer) &&
                    journey.userHasSeenCYAPage
                  then (journey, Redirect(checkYourAnswers)).asFuture
                  else
                    answer match {
                      case Yes =>
                        submitYes(securityDetail.securityDepositId, journey)
                      case No  =>
                        submitNo(securityDetail.securityDepositId, journey)
                    }

                }
                .getOrElse(Future.successful((journey, errorHandler.errorResult())))
          ),
    fastForwardToCYAEnabled = false
  )

  def submitYes(securityId: String, journey: SecuritiesJourney)(implicit
    request: Request[?]
  ): Future[(SecuritiesJourney, Result)] =
    journey
      .submitFullCorrectedAmounts(securityId)
      .fold(
        { error =>
          logger.warn(error)
          (journey, errorHandler.errorResult())
        },
        updatedJourney =>
          (
            updatedJourney,
            Redirect {
              if journey.userHasSeenCYAPage then routes.CheckYourAnswersController.show
              else if journey.getReasonForSecurity.exists(ntas.contains) then routes.ChooseExportMethodController.show
              else routes.ChoosePayeeTypeController.show
            }
          )
      )
      .asFuture

  def submitNo(securityId: String, journey: SecuritiesJourney): Future[(SecuritiesJourney, Result)] =
    (journey, Redirect(routes.PartialClaimsController.show)).asFuture
}
