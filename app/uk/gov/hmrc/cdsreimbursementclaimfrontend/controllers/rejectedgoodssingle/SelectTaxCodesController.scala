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

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectTaxCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectTaxCodesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectTaxCodesPage: pages.select_tax_codes
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val postAction: Call = routes.SelectTaxCodesController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val availableDuties: Seq[(TaxCode, Boolean)] = journey.getAvailableDuties

    if (availableDuties.isEmpty) {
      logger.warn("No available duties")
      Redirect(baseRoutes.IneligibleController.ineligible())
    } else {
      val form = selectTaxCodesForm(availableDuties.map(_._1)).withDefault(journey.getSelectedDuties)
      Ok(selectTaxCodesPage(form, availableDuties, None, postAction))
    }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val availableDuties: Seq[(TaxCode, Boolean)] = journey.getAvailableDuties
    Future.successful(if (availableDuties.isEmpty) {
      logger.warn("No available duties")
      (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
    } else {
      val form = selectTaxCodesForm(availableDuties.map(_._1))
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(selectTaxCodesPage(formWithErrors, availableDuties, None, postAction))
            ),
          taxCodesSelected =>
            (
              journey
                .selectAndReplaceTaxCodeSetForReimbursement(taxCodesSelected)
                .getOrElse(journey),
              Redirect(routes.EnterClaimController.show())
            )
        )
    })

  }
}

object SelectTaxCodesController {
  val selectTaxCodesKey: String = "select-duties"
}
