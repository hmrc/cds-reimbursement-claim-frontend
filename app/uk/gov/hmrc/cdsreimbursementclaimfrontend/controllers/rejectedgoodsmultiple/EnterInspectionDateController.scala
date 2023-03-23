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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterInspectionDateForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterInspectionDateController @Inject() (
  val jcc: JourneyControllerComponents,
  enterInspectionDatePage: pages.enter_inspection_date
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  val formKey: String          = "enter-inspection-date.rejected-goods"
  private val postAction: Call = routes.EnterInspectionDateController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      enterInspectionDatePage(enterInspectionDateForm.withDefault(journey.answers.inspectionDate), postAction)
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    enterInspectionDateForm
      .bindFromRequest()
      .fold(
        formWithErrors => (journey, BadRequest(enterInspectionDatePage(formWithErrors, postAction))),
        date => {
          val updatedJourney = journey.submitInspectionDate(date)
          (
            updatedJourney,
            if (updatedJourney.needsDeclarantAndConsigneePostCode)
              Redirect(routes.ChooseInspectionAddressTypeController.show())
            else
              Redirect(routes.ChooseInspectionAddressTypeController.redirectToALF())
          )
        }
      )
      .asFuture
  }
}
