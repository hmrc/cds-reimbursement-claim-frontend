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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterRejectedGoodsDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_rejected_goods_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterRejectedGoodsDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  enterRejectedGoodsDetailsPage: enter_rejected_goods_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadClaim { implicit request => claim =>
    val form = enterRejectedGoodsDetailsForm.withDefault(claim.answers.detailsOfRejectedGoods)
    Ok(enterRejectedGoodsDetailsPage(form, routes.EnterRejectedGoodsDetailsController.submit))
  }

  val submit: Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    enterRejectedGoodsDetailsForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            claim,
            BadRequest(
              enterRejectedGoodsDetailsPage(formWithErrors, routes.EnterRejectedGoodsDetailsController.submit)
            )
          ),
        rejectedGoodsDetails =>
          (
            claim.submitDetailsOfRejectedGoods(rejectedGoodsDetails),
            Redirect(routes.SelectDutiesController.showFirst)
          )
      )

  }
}
