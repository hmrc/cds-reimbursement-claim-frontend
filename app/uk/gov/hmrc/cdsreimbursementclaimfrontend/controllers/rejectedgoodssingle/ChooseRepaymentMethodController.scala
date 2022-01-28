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

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.reimbursementMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
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

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val filledForm = form.withDefault(journey.answers.reimbursementMethod)
    Ok(chooseReimbursementMethod(filledForm, postAction)).asFuture
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => (journey, BadRequest(chooseReimbursementMethod(formWithErrors, postAction))).asFuture,
        repaymentMethod =>
          (journey.submitReimbursementMethod(repaymentMethod), repaymentMethod) match {
            case (Right(updatedJourney), CurrentMonthAdjustment) =>
              (updatedJourney, Redirect(chooseFileTypeAction)).asFuture
            case (Right(updatedJourney), BankAccountTransfer)    =>
              (updatedJourney, Redirect("check-these-bank-details-are-correct")).asFuture
            case (Left(errorMessage), _)                         =>
              logger.error(s"We failed to choose the repayment method - $errorMessage")
              (journey, Redirect(routes.CheckYourAnswersController.show())).asFuture
          }
      )
  }
}
