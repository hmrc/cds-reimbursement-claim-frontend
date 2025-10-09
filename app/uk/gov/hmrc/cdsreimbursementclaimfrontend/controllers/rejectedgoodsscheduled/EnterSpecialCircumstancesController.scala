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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterSpecialCircumstancesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_special_circumstances

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterSpecialCircumstancesController @Inject() (
  val jcc: ClaimControllerComponents,
  enterSpecialCircumstancesPage: enter_special_circumstances
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledClaimBaseController
    with Logging {

  val formKey: String          = "enter-special-circumstances.rejected-goods"
  private val postAction: Call = routes.EnterSpecialCircumstancesController.submit

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  def show: Action[AnyContent] = actionReadClaim { implicit request => claim =>
    Ok(
      enterSpecialCircumstancesPage(
        enterSpecialCircumstancesForm.withDefault(claim.answers.basisOfClaimSpecialCircumstances),
        postAction
      )
    )
  }

  def submit: Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    enterSpecialCircumstancesForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            claim,
            BadRequest(
              enterSpecialCircumstancesPage(
                formWithErrors,
                postAction
              )
            )
          ),
        specialCircumstances =>
          claim
            .submitBasisOfClaimSpecialCircumstancesDetails(specialCircumstances)
            .fold(
              errors => {
                logger.error(s"unable to match basis of claim - $errors : ${claim.answers.basisOfClaim}")
                (claim, Redirect(baseRoutes.IneligibleController.ineligible))
              },
              updatedClaim => (updatedClaim, Redirect("choose-disposal-method")) // TODO: Fix with actual route
            )
      )

  }
}
