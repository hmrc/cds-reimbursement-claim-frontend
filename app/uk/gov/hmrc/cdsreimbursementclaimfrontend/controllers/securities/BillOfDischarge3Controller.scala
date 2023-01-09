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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectBillOfDischargeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BOD3
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_bill_of_discharge
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.invalid_bill_of_discharge

import scala.concurrent.ExecutionContext

@Singleton
class BillOfDischarge3Controller @Inject() (
  val jcc: JourneyControllerComponents,
  confirmBillOfDischarge: confirm_bill_of_discharge,
  invalidBillOfDischarge: invalid_bill_of_discharge
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  private val submitRoute = routes.BillOfDischarge3Controller.submit()

  def show(): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(confirmBillOfDischarge(selectBillOfDischargeForm, submitRoute, BOD3)).asFuture
  }

  def submit(): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    selectBillOfDischargeForm
      .bindFromRequest()
      .fold(
        formWithError => BadRequest(confirmBillOfDischarge(formWithError, submitRoute, BOD3)),
        {
          case Yes => Redirect(routes.SelectSecuritiesController.showFirst())
          case No  => Redirect(routes.BillOfDischarge3Controller.invalid())
        }
      )
      .asFuture
  }

  def invalid(): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(invalidBillOfDischarge(BOD3)).asFuture
  }
}
