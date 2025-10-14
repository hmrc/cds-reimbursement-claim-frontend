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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.basisOfRejectedGoodsClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim.SpecialCircumstances
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.select_basis_for_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class BasisForClaimController @Inject() (
  val jcc: ClaimControllerComponents,
  basisForClaimPage: select_basis_for_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val formKey: String = "select-basis-for-claim.rejected-goods"

  val show: Action[AnyContent] = actionReadClaim { claim =>
    val form: Form[BasisOfRejectedGoodsClaim] =
      basisOfRejectedGoodsClaimForm.withDefault(claim.answers.basisOfClaim)
    Ok(
      basisForClaimPage(
        form,
        BasisOfRejectedGoodsClaim.values,
        routes.BasisForClaimController.submit
      )
    )
  }

  val submit: Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    basisOfRejectedGoodsClaimForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            claim,
            BadRequest(
              basisForClaimPage(
                formWithErrors,
                BasisOfRejectedGoodsClaim.values,
                routes.BasisForClaimController.submit
              )
            )
          ),
        basisOfClaim =>
          (
            claim.submitBasisOfClaim(basisOfClaim),
            basisOfClaim match {
              case SpecialCircumstances =>
                Redirect(
                  routes.EnterSpecialCircumstancesController.show
                )
              case _                    => Redirect(routes.DisposalMethodController.show)
            }
          )
      )
  }
}
