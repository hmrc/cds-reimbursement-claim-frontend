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

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.basisOfRejectedGoodsClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim.SpecialCircumstances
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext

@Singleton
class BasisForClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  basisForClaimPage: pages.select_basis_for_claim
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  val formKey: String = "select-basis-for-claim.rejected-goods"

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val form = journey.answers.basisOfClaim.toList.foldLeft(basisOfRejectedGoodsClaimForm)((form, basisOfClaim) =>
      form.fill(basisOfClaim)
    )
    Ok(
      basisForClaimPage(
        form,
        BasisOfRejectedGoodsClaim.values,
        routes.BasisForClaimController.submit()
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
                routes.BasisForClaimController.submit()
              )
            )
          ).asFuture,
        basisOfClaim =>
          (
            journey.submitBasisOfClaim(basisOfClaim),
            basisOfClaim match {
              case SpecialCircumstances =>
                Redirect("/enter-special-circumstances") //FIXME: routes.EnterSpecialCircumstancesController.show()
              case _                    => Redirect(routes.DisposalMethodController.show())
            }
          ).asFuture
      )
  }
}
