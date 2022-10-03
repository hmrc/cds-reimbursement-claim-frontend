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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.implicits.catsSyntaxEq
import com.github.arturopala.validator.Validator.Validate

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.reimbursementMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext

@Singleton
class ChooseRepaymentMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseReimbursementMethod: pages.choose_repayment_method
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  private val form                 = reimbursementMethodForm("choose-payment-method.rejected-goods.single")
  private val postAction           = routes.ChooseRepaymentMethodController.submit()
  private val chooseFileTypeAction = routes.ChooseFileTypeController.show()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val filledForm = form.withDefault(journey.answers.reimbursementMethod)
    Ok(chooseReimbursementMethod(filledForm, postAction)).asFuture
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors => (journey, BadRequest(chooseReimbursementMethod(formWithErrors, postAction))),
          repaymentMethod =>
            (journey.submitReimbursementMethod(repaymentMethod), repaymentMethod) match {
              case (Right(updatedJourney), CurrentMonthAdjustment) =>
                if (journey.userHasSeenCYAPage) (updatedJourney, Redirect(checkYourAnswers))
                else (updatedJourney, Redirect(chooseFileTypeAction))
              case (Right(updatedJourney), BankAccountTransfer)    =>
                if (journey.userHasSeenCYAPage && (journey.answers.reimbursementMethod === Some(repaymentMethod)))
                  (updatedJourney, Redirect(checkYourAnswers))
                else (updatedJourney, Redirect(routes.CheckBankDetailsController.show()))
              case (Left(errorMessage), _)                         =>
                logger.error(s"We failed to choose the repayment method - $errorMessage")
                (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
            }
        )
        .asFuture
    },
    fastForwardToCYAEnabled = false
  )

  def reset(): Action[AnyContent] = actionReadWriteJourney { _ => journey =>
    val updatedJourney =
      if (!journey.isAllSelectedDutiesAreCMAEligible) journey.resetReimbursementMethod()
      else journey
    (updatedJourney, Redirect(checkYourAnswers)).asFuture
  }
}
