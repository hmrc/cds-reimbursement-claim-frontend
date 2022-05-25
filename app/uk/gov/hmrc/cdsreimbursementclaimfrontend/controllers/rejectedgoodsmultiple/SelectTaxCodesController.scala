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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectTaxCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.mrn_does_not_exist
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.select_tax_codes

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class SelectTaxCodesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectTaxCodesPage: select_tax_codes,
  mrnDoesNotExistPage: mrn_does_not_exist
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  val showFirst: Action[AnyContent] = show(1)

  def show(pageIndex: Int): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey
      .getNthMovementReferenceNumber(pageIndex - 1)
      .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
        val availableDuties: Seq[(TaxCode, Boolean)] = journey.getAvailableDuties(mrn)
        if (availableDuties.isEmpty) {
          logger.warn("No available duties")
          Redirect(baseRoutes.IneligibleController.ineligible())
        } else {
          val form = selectTaxCodesForm(availableDuties.map(_._1)).withDefault(journey.getSelectedDuties(mrn))
          Ok(
            selectTaxCodesPage(
              form,
              availableDuties,
              Some((pageIndex, mrn)),
              false,
              MRNMultipleRoutes.subKey,
              routes.SelectTaxCodesController.submit(pageIndex)
            )
          )
        }
      }
      .asFuture
  }

  def submit(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      journey
        .getNthMovementReferenceNumber(pageIndex - 1)
        .fold((journey, BadRequest(mrnDoesNotExistPage()))) { mrn =>
          val availableDuties: Seq[(TaxCode, Boolean)] = journey.getAvailableDuties(mrn)
          if (availableDuties.isEmpty) {
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
                    BadRequest(
                      selectTaxCodesPage(
                        formWithErrors,
                        availableDuties,
                        Some((pageIndex, mrn)),
                        false,
                        MRNMultipleRoutes.subKey,
                        routes.SelectTaxCodesController.submit(pageIndex)
                      )
                    )
                  ),
                taxCodesSelected =>
                  (
                    journey
                      .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodesSelected)
                      .getOrElse(journey),
                    Redirect(
                      taxCodesSelected.headOption match {
                        case Some(taxCode) => routes.EnterClaimController.show(pageIndex, taxCode)
                        case None          => routes.SelectTaxCodesController.show(pageIndex)
                      }
                    )
                  )
              )
          }
        }
        .asFuture
    },
    fastForwardToCYAEnabled = false
  )
}

object SelectTaxCodesController {
  val selectTaxCodesKey: String = "select-duties"
}
