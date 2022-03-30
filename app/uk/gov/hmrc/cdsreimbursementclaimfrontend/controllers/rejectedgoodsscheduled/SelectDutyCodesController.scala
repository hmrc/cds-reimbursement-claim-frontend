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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutyCodesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutyCodesPage: pages.select_duty_codes
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  val iterate: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    def selectDuties: Future[Result] = Redirect(routes.SelectDutyTypesController.show()).asFuture

    def start(dutyType: DutyType): Future[Result] = Redirect(routes.SelectDutyCodesController.show(dutyType)).asFuture

    journey.answers.reimbursementClaims.flatMap(_.value.headOption).fold(selectDuties) {
      selectedDutyTaxCodesReimbursement => start(selectedDutyTaxCodesReimbursement._1)
    }
  }

  def show(dutyType: DutyType): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val postAction: Call = routes.SelectDutyCodesController.submit(dutyType)

    val maybeTaxCodes: Option[List[TaxCode]] = Option(journey.getSelectedDuties(dutyType).toList)
    val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)

    Ok(selectDutyCodesPage(dutyType, form, postAction)).asFuture

  }

  def submit(currentDuty: DutyType): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.SelectDutyCodesController.submit(currentDuty)

    Future.successful(
      selectDutyCodesForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(selectDutyCodesPage(currentDuty, formWithErrors, postAction))
            ),
          selectedTaxCodes =>
            journey
              .selectAndReplaceTaxCodeSetForReimbursement(currentDuty, selectedTaxCodes)
              .fold(
                errors => {
                  logger.error(s"Error updating tax codes selection - $errors")
                  (journey, Redirect(routes.SelectDutyCodesController.show(currentDuty)))
                },
                updatedJourney =>
                  (
                    updatedJourney,
                    updatedJourney.findNextSelectedDutyAfter(currentDuty) match {
                      case Some(nextDuty) => Redirect(routes.SelectDutyCodesController.show(nextDuty))
                      case None           =>
                        Redirect(
                          "/rejected-goods/scheduled/select-duties/reimbursement-claim/start"
                        ) //FIXME: routes.EnterScheduledClaimController.iterate()
                    }
                  )
              )
        )
    )
  }

}
