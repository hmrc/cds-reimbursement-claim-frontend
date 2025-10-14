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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterAdditionalDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.enter_additional_details

import scala.concurrent.Future

trait OverpaymentsEnterAdditionalDetailsMixin extends ClaimBaseController {

  type Claim <: claims.Claim & claims.ClaimBase & claims.OverpaymentsClaimProperties

  val enterAdditionalDetailsPage: enter_additional_details
  val postAction: Call
  val continueRoute: Call

  def modifyClaim(claim: Claim, additionalDetails: String): Claim

  final val show: Action[AnyContent] =
    actionReadClaim { claim =>
      Future.successful {
        val form: Form[String] =
          enterAdditionalDetailsForm.withDefault(claim.answers.additionalDetails)
        Ok(
          enterAdditionalDetailsPage(
            form,
            postAction
          )
        )
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      enterAdditionalDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                claim,
                BadRequest(
                  enterAdditionalDetailsPage(
                    formWithErrors,
                    postAction
                  )
                )
              )
            ),
          additionalDetails =>
            Future.successful(
              (
                modifyClaim(claim, additionalDetails),
                Redirect(continueRoute)
              )
            )
        )
    }

}
