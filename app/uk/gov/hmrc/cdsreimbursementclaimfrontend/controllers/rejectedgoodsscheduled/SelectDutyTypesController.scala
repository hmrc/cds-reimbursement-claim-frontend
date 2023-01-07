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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import com.github.arturopala.validator.Validator.Validate

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyTypesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.concurrent.ExecutionContext

@Singleton
class SelectDutyTypesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutyTypesPage: pages.select_duty_types
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  val postAction: Call = routes.SelectDutyTypesController.submit()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    val form = selectDutyTypesForm

    Ok(selectDutyTypesPage(form, postAction)).asFuture

  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    selectDutyTypesForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              selectDutyTypesPage(
                formWithErrors,
                postAction
              )
            )
          ),
        dutyTypes =>
          journey
            .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
            .fold(
              errors => {
                logger.error(s"Error updating duty types selection - $errors")
                (journey, BadRequest(selectDutyTypesPage(selectDutyTypesForm, postAction)))
              },
              updatedJourney =>
                (
                  updatedJourney,
                  Redirect(
                    routes.SelectTaxCodesController
                      .show(dutyTypes.headOption.getOrElse(throw new Exception("Unexpected empty duty types")))
                  )
                )
            )
      )
      .asFuture
  }

}
