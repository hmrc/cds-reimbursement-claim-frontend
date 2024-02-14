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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.basisOfRejectedGoodsClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim.SpecialCircumstances
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.select_basis_for_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class BasisForClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  basisForClaimPage: select_basis_for_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsMultipleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val formKey: String = "select-basis-for-claim.rejected-goods"

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val form: Form[BasisOfRejectedGoodsClaim] =
      basisOfRejectedGoodsClaimForm.withDefault(journey.answers.basisOfClaim)
    Ok(
      basisForClaimPage(
        form,
        BasisOfRejectedGoodsClaim.values,
        routes.BasisForClaimController.submit
      )
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    basisOfRejectedGoodsClaimForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              basisForClaimPage(
                formWithErrors,
                BasisOfRejectedGoodsClaim.values,
                routes.BasisForClaimController.submit
              )
            )
          ).asFuture,
        basisOfClaim =>
          (
            journey.submitBasisOfClaim(basisOfClaim),
            basisOfClaim match {
              case SpecialCircumstances =>
                Redirect(
                  routes.EnterSpecialCircumstancesController.show
                )
              case _                    => Redirect(routes.DisposalMethodController.show)
            }
          ).asFuture
      )
  }
}
