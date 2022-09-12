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
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.checkTotalImportDischargedForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_total_import_discharged_page

import scala.concurrent.ExecutionContext

@Singleton
class CheckTotalImportDischargedController @Inject() (
  val jcc: JourneyControllerComponents,
  checkTotalImportDischargedPage: check_total_import_discharged_page
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {
  private val form: Form[YesNo] = checkTotalImportDischargedForm

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  //Success: Declaration has been found and ReasonForSecurity is InwardProcessingRelief.
  private val successResultBOD3: Result =
    Redirect(routes.BillOfDischarge3Controller.show())

  //Success: Declaration has been found and ReasonForSecurity is EndUseRelief.
  private val successResultBOD4: Result =
    Redirect(routes.BillOfDischarge4Controller.show())

  def show(): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(checkTotalImportDischargedPage(form, routes.CheckTotalImportDischargedController.submit())).asFuture
  }

  def submit(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    form.bindFromRequest
      .fold(
        formWithErrors =>
          BadRequest(
            checkTotalImportDischargedPage(formWithErrors, routes.CheckTotalImportDischargedController.submit())
          ).asFuture,
        {
          case Yes =>
            {
              if (journey.reasonForSecurityIsIPR) successResultBOD3
              else if (journey.reasonForSecurityIsEndUseRelief) successResultBOD4
              else {
                logAndDisplayError(
                  "Invalid journey routing",
                  s"Reason for security [${journey.getReasonForSecurity}] must be one of [InwardProcessingRelief, EndUseRelief]"
                )
              }
            }.asFuture
          case No  => Redirect(routes.ClaimInvalidNotExportedAllController.show()).asFuture
        }
      )

  }
}
