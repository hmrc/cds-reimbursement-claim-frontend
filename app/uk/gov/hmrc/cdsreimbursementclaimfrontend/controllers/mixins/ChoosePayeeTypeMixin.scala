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
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.payeeTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_payee_type

import scala.concurrent.Future

trait ChoosePayeeTypeMixin extends ClaimBaseController {

  def modifyClaim(claim: Claim, payeeType: PayeeType): Either[String, Claim]
  val choosePayeeTypePage: choose_payee_type
  val postAction: Call
  def nextPage(claim: Claim): Call

  final def submitPayeeType(payeeType: PayeeType)(implicit claim: Claim): (Claim, Result) =
    modifyClaim(claim, payeeType)
      .fold(
        e => {
          logger.warn(e)
          (claim, Redirect(baseRoutes.IneligibleController.ineligible))
        },
        (_, Redirect(nextPage(claim)))
      )

  final val show: Action[AnyContent] =
    actionReadWriteClaim { implicit request => implicit claim =>
      if claim.needsPayeeTypeSelection
      then {
        val form: Form[PayeeType] =
          payeeTypeForm.withDefault(claim.answers.payeeType)
        (claim, Ok(choosePayeeTypePage(form, postAction)))
      } else {
        (claim, Redirect(nextPage(claim)))
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { implicit request => implicit claim =>
      payeeTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                claim,
                BadRequest(
                  choosePayeeTypePage(formWithErrors, postAction)
                )
              )
            ),
          submitPayeeType
        )
    }
}
