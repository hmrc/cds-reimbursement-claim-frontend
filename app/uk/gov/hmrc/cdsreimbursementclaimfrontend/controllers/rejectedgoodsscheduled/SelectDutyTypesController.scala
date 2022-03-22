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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyTypesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.concurrent.ExecutionContext

@Singleton
class SelectDutyTypesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutyTypesPage: pages.select_duty_types
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  val formKey: String = "select-duty-types"

  //TODO: move form to Forms file
  //TODO: tests
  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    val form = selectDutyTypesForm

    Ok(selectDutyTypesPage(form)).asFuture

  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    selectDutyTypesForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              selectDutyTypesPage(
                formWithErrors
              )
            )
          ),
        dutyTypes =>
          (
            journey.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrElse(journey),
            Redirect(" /select-duties/:category page") //FIXME: routes.SelectDutyCodesController.iterate()
          )
      )
      .asFuture
  }

}
