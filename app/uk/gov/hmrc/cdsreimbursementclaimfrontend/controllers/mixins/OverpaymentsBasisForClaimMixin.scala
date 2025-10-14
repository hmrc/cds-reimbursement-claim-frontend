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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.basisOfOverpaymentClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.select_basis_for_claim

import scala.concurrent.Future

trait OverpaymentsBasisForClaimMixin extends ClaimBaseController {

  type Claim <: claims.Claim & claims.ClaimBase & claims.OverpaymentsClaimProperties

  val basisForClaimPage: select_basis_for_claim
  val postAction: Call
  val featureSwitchService: FeatureSwitchService
  def continueRoute(basisOfClaim: BasisOfOverpaymentClaim): Call

  def modifyClaim(claim: Claim, basisOfClaim: BasisOfOverpaymentClaim): Claim

  val formKey: String = "select-basis-for-claim"

  final val show: Action[AnyContent] =
    actionReadClaim { claim =>
      Future.successful {
        val form: Form[BasisOfOverpaymentClaim] =
          basisOfOverpaymentClaimForm.withDefault(claim.answers.basisOfClaim)
        Ok(
          basisForClaimPage(
            form,
            claim.getAvailableClaimTypes,
            DropdownHints(
              claim.getAvailableClaimTypes.toList.sorted
                .map(_.toString)
            ),
            None,
            postAction
          )
        )
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      basisOfOverpaymentClaimForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                claim,
                BadRequest(
                  basisForClaimPage(
                    formWithErrors,
                    claim.getAvailableClaimTypes,
                    DropdownHints(
                      claim.getAvailableClaimTypes.toList.sorted
                        .map(_.toString)
                    ),
                    None,
                    postAction
                  )
                )
              )
            ),
          basisOfClaim =>
            Future.successful(
              (
                modifyClaim(claim, basisOfClaim),
                Redirect(continueRoute(basisOfClaim))
              )
            )
        )
    }
}
