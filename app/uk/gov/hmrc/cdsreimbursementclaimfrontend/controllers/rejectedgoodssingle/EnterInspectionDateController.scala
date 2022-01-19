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
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterInspectionDateForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterInspectionDateController @Inject() (
  val jcc: JourneyControllerComponents,
  enterInspectionDatePage: pages.enter_inspection_date
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val formKey: String          = "enter-inspection-date.rejected-goods"
  private val postAction: Call = routes.EnterInspectionDateController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      val form = enterInspectionDateForm.withDefault(journey.answers.inspectionDate)
      Ok(enterInspectionDatePage(form, postAction))
    }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    Future.successful(
      enterInspectionDateForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterInspectionDatePage(
                  formWithErrors,
                  postAction
                )
              )
            ),
          date =>
            (
              journey.submitInspectionDate(date),
              Redirect("inspection-address") //FIXME
            )
        )
    )
  }

}
