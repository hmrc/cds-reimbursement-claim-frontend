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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_duty_codes

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutyCodesPage: select_duty_codes
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  private val selectDutiesAction: Call = routes.SelectDutyTypesController.show

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  def show(dutyType: DutyType): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val isSubsidy: Boolean = journey.isSubsidyOnlyJourney

    if journey.isDutyTypeSelected then {
      val postAction: Call                     = routes.SelectDutiesController.submit(dutyType)
      val maybeTaxCodes: Option[List[TaxCode]] = Option(journey.getSelectedDuties(dutyType).toList)
      val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)

      Ok(selectDutyCodesPage(dutyType, form, postAction, isSubsidy)).asFuture
    } else {
      Redirect(selectDutiesAction).asFuture
    }
  }

  def submit(currentDuty: DutyType): Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      journey => {
        val postAction: Call = routes.SelectDutiesController.submit(currentDuty)
        if journey.isDutyTypeSelected then {
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
                          selectedTaxCodes.headOption.fold(
                            BadRequest(selectDutyCodesPage(currentDuty, selectDutyCodesForm, postAction))
                          )(taxCode => Redirect(routes.EnterClaimController.show(currentDuty, taxCode)))
                        )
                    )
              )
          )
        } else {
          (journey, Redirect(selectDutiesAction)).asFuture
        }
      },
    fastForwardToCYAEnabled = false
  )
}
