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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectBillOfDischargeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
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

  private val form: Form[YesNo] = selectBillOfDischargeForm

  private val submitUrl = routes.BillOfDischargeController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(confirmBillOfDischarge(form, submitUrl)).asFuture
  }

  val submit: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    form
      .bindFromRequest()
      .fold(
        formWithError => BadRequest(confirmBillOfDischarge(formWithError, submitUrl)),
        {
          case Yes => Redirect(routes.SelectSecuritiesController.showFirst())
          case No  => Redirect(routes.BillOfDischargeController.invalid())
        }
      )
      .asFuture
  }

  val invalid: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(invalidBillOfDischarge()).asFuture
  }
}
