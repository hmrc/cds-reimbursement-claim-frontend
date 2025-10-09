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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.select_duties

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: ClaimControllerComponents,
  selectDutiesPage: select_duties
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleClaimBaseController {

  val postAction: Call = routes.SelectDutiesController.submit

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val show: Action[AnyContent] = actionReadClaim { implicit request => claim =>
    Future {
      val availableDuties: Seq[(TaxCode, Boolean)] = claim.getAvailableDuties

      if availableDuties.isEmpty then {
        logger.warn("No available duties")
        Redirect(baseRoutes.IneligibleController.ineligible)
      } else {
        val form = selectDutiesForm(availableDuties.map(_._1)).withDefault(claim.getSelectedDuties)
        Ok(
          selectDutiesPage(
            form = form,
            availableTaxCodes = availableDuties,
            indexAndMrnOpt = None,
            showCmaNotEligibleHint = true,
            subKey = Some("single"),
            postAction = postAction,
            guidanceUrl = Some(viewConfig.rejectedGoodsOverpaidVatGuidanceUrl)
          )
        )
      }
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim => {
        val availableDuties: Seq[(TaxCode, Boolean)] = claim.getAvailableDuties
        Future.successful(if availableDuties.isEmpty then {
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
                      formWithErrors,
                      availableDuties,
                      None,
                      true,
                      Some("single"),
                      postAction,
                      Some(viewConfig.rejectedGoodsOverpaidVatGuidanceUrl)
                    )
                  )
                ),
              taxCodesSelected =>
                (
                  claim
                    .selectAndReplaceTaxCodeSetForReimbursement(taxCodesSelected)
                    .getOrElse(claim),
                  Redirect(routes.EnterClaimController.showFirst)
                )
            )
        })
      },
    fastForwardToCYAEnabled = false
  )
}

object SelectDutiesController {
  val selectDutiesKey: String = "select-duties"
}
