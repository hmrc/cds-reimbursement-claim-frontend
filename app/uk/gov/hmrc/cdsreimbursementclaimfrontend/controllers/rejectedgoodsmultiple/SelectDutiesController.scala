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

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.mrn_does_not_exist
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.select_duties

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutiesPage: select_duties,
  mrnDoesNotExistPage: mrn_does_not_exist
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

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
          val form = selectDutiesForm(availableDuties.map(_._1)).withDefault(journey.getSelectedDuties(mrn))
          Ok(
            selectDutiesPage(
              form,
              availableDuties,
              Some((pageIndex, mrn)),
              false,
              Some("multiple"),
              routes.SelectDutiesController.submit(pageIndex),
              isSubsidyOnly = journey.isSubsidyOnlyJourney
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
            val form = selectDutiesForm(availableDuties.map(_._1))
            form
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  (
                    journey,
                    BadRequest(
                      selectDutiesPage(
                        formWithErrors,
                        availableDuties,
                        Some((pageIndex, mrn)),
                        false,
                        Some("multiple"),
                        routes.SelectDutiesController.submit(pageIndex),
                        isSubsidyOnly = journey.isSubsidyOnlyJourney
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
                        case None          => routes.SelectDutiesController.show(pageIndex)
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
