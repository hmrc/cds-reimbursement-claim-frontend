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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.mrn_does_not_exist
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.select_duties

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class SelectDutiesController @Inject() (
  val jcc: ClaimControllerComponents,
  selectDutiesPage: select_duties,
  mrnDoesNotExistPage: mrn_does_not_exist
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val showFirst: Action[AnyContent] = show(1)

  def show(pageIndex: Int): Action[AnyContent] = actionReadClaim { claim =>
    claim
      .getNthMovementReferenceNumber(pageIndex - 1)
      .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
        val availableDuties: Seq[(TaxCode, Boolean)] = claim.getAvailableDuties(mrn)
        if availableDuties.isEmpty then {
          logger.warn("No available duties")
          Redirect(baseRoutes.IneligibleController.ineligible)
        } else {
          val form = selectDutiesForm(availableDuties.map(_._1)).withDefault(claim.getSelectedDuties(mrn))
          Ok(
            selectDutiesPage(
              form = form,
              availableTaxCodes = availableDuties,
              indexAndMrnOpt = Some((pageIndex, mrn)),
              showCmaNotEligibleHint = false,
              subKey = Some("multiple"),
              postAction = routes.SelectDutiesController.submit(pageIndex)
            )
          )
        }
      }

  }

  def submit(pageIndex: Int): Action[AnyContent] = actionReadWriteClaim(
    claim =>
      claim
        .getNthMovementReferenceNumber(pageIndex - 1)
        .fold((claim, BadRequest(mrnDoesNotExistPage()))) { mrn =>
          val availableDuties: Seq[(TaxCode, Boolean)] = claim.getAvailableDuties(mrn)
          if availableDuties.isEmpty then {
            logger.warn("No available duties")
            (claim, Redirect(baseRoutes.IneligibleController.ineligible))
          } else {
            val form = selectDutiesForm(availableDuties.map(_._1))
            form
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  (
                    claim,
                    BadRequest(
                      selectDutiesPage(
                        form = formWithErrors,
                        availableTaxCodes = availableDuties,
                        indexAndMrnOpt = Some((pageIndex, mrn)),
                        showCmaNotEligibleHint = false,
                        subKey = Some("multiple"),
                        postAction = routes.SelectDutiesController.submit(pageIndex)
                      )
                    )
                  ),
                taxCodesSelected =>
                  (
                    claim
                      .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodesSelected)
                      .getOrElse(claim),
                    Redirect(
                      taxCodesSelected.headOption match {
                        case Some(taxCode) => routes.EnterClaimController.show(pageIndex, taxCode)
                        case None          => routes.SelectDutiesController.show(pageIndex)
                      }
                    )
                  )
              )
          }
        },
    fastForwardToCYAEnabled = false
  )
}
