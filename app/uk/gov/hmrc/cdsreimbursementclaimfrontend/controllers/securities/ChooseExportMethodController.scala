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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.chooseExportMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.confirmFullRepaymentForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.WorkInProgressMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_export_method
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseExportMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseExportMethodPage: choose_export_method
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val form: Form[Option[TemporaryAdmissionMethodOfDisposal]] = chooseExportMethodForm

  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      chooseExportMethodPage(
        chooseExportMethodForm,
        TemporaryAdmissionMethodOfDisposal.orderedValues.toList,
        routes.ChooseExportMethodController.submit()
      )
    ).asFuture
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form.bindFromRequest.fold(
      formWithErrors =>
        (
          journey,
          BadRequest(
            chooseExportMethodPage(
              formWithErrors,
              TemporaryAdmissionMethodOfDisposal.orderedValues.toList,
              routes.ChooseExportMethodController.submit()
            )
          )
        ).asFuture,
      {
        case None                   =>
          logger.warn("no value was submitted for TemporaryAdmissionMethodOfDisposal, but there were no form errors")
          (journey, errorHandler.errorResult()).asFuture
        case Some(methodOfDisposal) =>
          journey
            .submitTemporaryAdmissionMethodOfDisposal(methodOfDisposal)
            .fold(
              error => {
                logger.warn(error)
                (journey, errorHandler.errorResult()).asFuture
              },
              updatedJourney => (updatedJourney, Redirect(routes.CheckClaimantDetailsController.show())).asFuture
            )
      }
    )
  }
}
