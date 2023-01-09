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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_basis_for_claim

import scala.concurrent.Future

trait OverpaymentsBasisForClaimMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with journeys.JourneyBase with journeys.OverpaymentsJourneyProperties

  val basisForClaimPage: select_basis_for_claim
  val postAction: Call
  def continueRoute(basisOfClaim: BasisOfOverpaymentClaim): Call

  def modifyJourney(journey: Journey, basisOfClaim: BasisOfOverpaymentClaim): Journey

  val formKey: String = "select-basis-for-claim"

  private val basisOfClaimsHints: DropdownHints =
    DropdownHints.range(elementIndex = 0, maxHints = 14)

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      Future.successful {
        val form: Form[BasisOfOverpaymentClaim] =
          basisOfOverpaymentClaimForm.withDefault(journey.answers.basisOfClaim)
        Ok(
          basisForClaimPage(
            form,
            journey.getAvailableClaimTypes,
            basisOfClaimsHints,
            None,
            postAction
          )
        )
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      basisOfOverpaymentClaimForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  basisForClaimPage(
                    formWithErrors,
                    journey.getAvailableClaimTypes,
                    basisOfClaimsHints,
                    None,
                    postAction
                  )
                )
              )
            ),
          basisOfClaim =>
            Future.successful(
              (
                modifyJourney(journey, basisOfClaim),
                Redirect(continueRoute(basisOfClaim))
              )
            )
        )
    }
}
