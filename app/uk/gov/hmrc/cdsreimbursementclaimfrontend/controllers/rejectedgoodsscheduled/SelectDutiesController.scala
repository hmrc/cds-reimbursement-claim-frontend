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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutyCodesPage: pages.select_duty_codes
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  def show(dutyType: DutyType): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    if (journey.isDutyTypeSelected) {
      val postAction: Call                     = routes.SelectDutiesController.submit(dutyType)
      val maybeTaxCodes: Option[List[TaxCode]] = Option(journey.getSelectedDuties(dutyType).toList)
      val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)

      Ok(selectDutyCodesPage(dutyType, form, postAction)).asFuture
    } else {
      Redirect(routes.SelectDutyTypesController.show()).asFuture
    }

  }

  def submit(currentDuty: DutyType): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.SelectDutiesController.submit(currentDuty)
    if (journey.isDutyTypeSelected) {
      Future.successful(
        selectDutyCodesForm
          .bindFromRequest()
          .fold(
            formWithErrors => (journey, BadRequest(selectDutyCodesPage(currentDuty, formWithErrors, postAction))),
            selectedTaxCodes =>
              journey
                .selectAndReplaceTaxCodeSetForReimbursement(currentDuty, selectedTaxCodes)
                .fold(
                  errors => {
                    logger.error(s"Error updating tax codes selection - $errors")
                    (journey, BadRequest(selectDutyCodesPage(currentDuty, selectDutyCodesForm, postAction)))
                  },
                  updatedJourney =>
                    (
                      updatedJourney,
                      updatedJourney.findNextSelectedDutyAfter(currentDuty) match {
                        case Some(nextDuty) => Redirect(routes.SelectDutiesController.show(nextDuty))
                        case None           => Redirect(routes.EnterClaimController.showFirst())
                      }
                    )
                )
          )
      )
    } else {
      (journey, Redirect(routes.SelectDutyTypesController.show())).asFuture
    }

  }

}
