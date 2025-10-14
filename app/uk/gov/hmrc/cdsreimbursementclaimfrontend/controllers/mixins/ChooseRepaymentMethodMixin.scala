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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_repayment_method

import scala.concurrent.Future

trait ChooseRepaymentMethodMixin extends ClaimBaseController {

  type Claim <: claims.Claim & claims.ClaimBase & claims.SingleVariantProperties

  def postAction: Call
  def enterBankDetailsRoute: Call
  def chooseRepaymentMethodPage: choose_repayment_method

  def modifyClaim(claim: Claim, method: ReimbursementMethod): Either[String, Claim]
  def resetReimbursementMethod(claim: Claim): Claim

  val form: Form[ReimbursementMethod] =
    Forms.reimbursementMethodForm("reimbursement-method")

  final val show: Action[AnyContent] =
    actionReadClaim { implicit claim =>
      if claim.isAllSelectedDutiesAreCMAEligible then {
        Ok(
          chooseRepaymentMethodPage(
            form.withDefault(claim.answers.reimbursementMethod),
            postAction
          )
        )
      } else Redirect(enterBankDetailsRoute)
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim(
      implicit request =>
        claim =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  claim,
                  BadRequest(
                    chooseRepaymentMethodPage(
                      formWithErrors,
                      postAction
                    )
                  )
                ),
              method =>
                modifyClaim(claim, method) match {
                  case Right(modifiedClaim) =>
                    (
                      modifiedClaim,
                      Redirect(enterBankDetailsRoute)
                    )

                  case Left("submitReimbursementMethod.notCMAEligible") =>
                    (
                      claim,
                      Redirect(enterBankDetailsRoute)
                    )

                  case Left(error) =>
                    Future.failed(new Exception(error))
                }
            ),
      fastForwardToCYAEnabled = false
    )

  final val reset: Action[AnyContent] =
    actionReadWriteClaim { _ => claim =>
      val updatedClaim =
        if !claim.isAllSelectedDutiesAreCMAEligible then resetReimbursementMethod(claim)
        else claim
      (updatedClaim, Redirect(checkYourAnswers))
    }
}
