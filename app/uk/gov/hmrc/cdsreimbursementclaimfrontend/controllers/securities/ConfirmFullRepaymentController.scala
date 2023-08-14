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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.SummaryListCreator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ConfirmFullRepaymentController @Inject() (
  val jcc: JourneyControllerComponents,
  confirmFullRepaymentPage: confirm_full_repayment
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val form: Form[YesNo] = confirmFullRepaymentForm

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def showFirst(): Action[AnyContent] = actionReadJourney { _ => journey =>
    journey.getSelectedDepositIds.headOption
      .fold(
        Redirect(routes.CheckDeclarationDetailsController.show()).asFuture
      )(id => Redirect(routes.ConfirmFullRepaymentController.show(id)).asFuture)
  }

  private def getIndexAndSummaryList(displayDeclaration: DisplayDeclaration, id: String): (Int, SummaryList) =
    (
      displayDeclaration.getSecurityDepositIdIndex(id) + 1,
      getSummaryList(displayDeclaration, id)
    )

  private def getSummaryList(displayDeclaration: DisplayDeclaration, id: String): SummaryList =
    SummaryListCreator.create(
      ("Security deposit ID", id),
      ("Deposit value", displayDeclaration.getSecurityTotalValueFor(id).toPoundSterlingString)
    )

  def show(id: String): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    journey
      .getDisplayDeclarationIfValidSecurityDepositId(id)
      .map(getIndexAndSummaryList(_, id))
      .fold((journey, errorHandler.errorResult())) { case (index, summaryList) =>
        (
          journey.resetClaimFullAmountMode(),
          Ok(
            confirmFullRepaymentPage(
              form.withDefault(journey.getClaimFullAmountStatus(id)),
              id,
              index,
              journey.getSelectedDepositIds.length,
              summaryList,
              routes.ConfirmFullRepaymentController.submit(id)
            )
          )
        )
      }
      .asFuture
  }

  def submit(id: String): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              journey
                .getDisplayDeclarationIfValidSecurityDepositId(id)
                .map(getIndexAndSummaryList(_, id))
                .map { case (index, summaryList) =>
                  BadRequest(
                    confirmFullRepaymentPage(
                      formWithErrors,
                      id,
                      index,
                      journey.getSelectedDepositIds.length,
                      summaryList,
                      routes.ConfirmFullRepaymentController.submit(id)
                    )
                  )
                }
                .getOrElse(errorHandler.errorResult())
            ).asFuture,
          answer =>
            if (
              journey.getClaimFullAmountStatus(id).contains(answer) &&
              journey.userHasSeenCYAPage
            )
              (journey, Redirect(checkYourAnswers)).asFuture
            else
              answer match {
                case Yes =>
                  submitYes(id, journey)
                case No  =>
                  submitNo(id, journey)
              }
        )
    },
    fastForwardToCYAEnabled = false
  )

  def submitYes(securityId: String, journey: SecuritiesJourney)(implicit
    request: Request[_]
  ): Future[(SecuritiesJourney, Result)] =
    journey
      .submitFullAmountsForReclaim(securityId)
      .fold(
        { error =>
          logger.warn(error)
          (journey, errorHandler.errorResult())
        },
        { updatedJourney =>
          val nextRoute =
            if (journey.answers.checkClaimDetailsChangeMode)
              routes.CheckClaimDetailsController.show()
            else
              journey.getSelectedDepositIds
                .nextAfter(securityId)
                .fold(routes.CheckClaimDetailsController.show()) { nextSecurityId =>
                  routes.ConfirmFullRepaymentController.show(nextSecurityId)
                }
          (updatedJourney, Redirect(nextRoute))
        }
      )
      .asFuture

  def submitNo(securityId: String, journey: SecuritiesJourney): Future[(SecuritiesJourney, Result)] =
    (if (journey.getSelectedDutiesFor(securityId).isEmpty || journey.isFullSecurityAmountClaimed(securityId))
       (
         journey.submitClaimFullAmountMode(false),
         Redirect(routes.SelectDutiesController.show(securityId))
       )
     else
       (
         journey,
         Redirect(routes.CheckClaimDetailsController.show())
       )).asFuture
}
