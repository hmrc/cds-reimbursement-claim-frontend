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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectBillOfDischargeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BOD3
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BOD4
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BillOfDischarge
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_bill_of_discharge
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.invalid_bill_of_discharge

import scala.concurrent.ExecutionContext

@Singleton
class BillOfDischargeController @Inject() (
  val jcc: JourneyControllerComponents,
  confirmBillOfDischarge: confirm_bill_of_discharge,
  invalidBillOfDischarge: invalid_bill_of_discharge
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  val showBOD3: Action[AnyContent] = show(BOD3)
  val showBOD4: Action[AnyContent] = show(BOD4)

  val submitBOD3: Action[AnyContent] = submit(BOD3)
  val submitBOD4: Action[AnyContent] = submit(BOD4)

  val invalidBOD3: Action[AnyContent] = invalid(BOD3)
  val invalidBOD4: Action[AnyContent] = invalid(BOD4)

  private def show(implicit bod: BillOfDischarge): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(confirmBillOfDischarge(selectBillOfDischargeForm, submitUrl, bod)).asFuture
  }

  private def submit(implicit bod: BillOfDischarge): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    selectBillOfDischargeForm
      .bindFromRequest()
      .fold(
        formWithError => BadRequest(confirmBillOfDischarge(formWithError, submitUrl, bod)),
        {
          case Yes => Redirect(routes.SelectSecuritiesController.showFirst())
          case No  =>
            bod match {
              case BOD3 => Redirect(routes.BillOfDischargeController.invalidBOD3())
              case BOD4 => Redirect(routes.BillOfDischargeController.invalidBOD4())
            }
        }
      )
      .asFuture
  }

  private def invalid(bod: BillOfDischarge): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(invalidBillOfDischarge(bod)).asFuture
  }

  private def submitUrl(implicit bod: BillOfDischarge): Call = bod match {
    case BOD3 => routes.BillOfDischargeController.submitBOD3()
    case BOD4 => routes.BillOfDischargeController.submitBOD4()
  }
}
