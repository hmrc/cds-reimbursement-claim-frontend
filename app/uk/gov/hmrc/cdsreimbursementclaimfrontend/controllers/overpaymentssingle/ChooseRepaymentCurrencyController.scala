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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CurrencyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_currency_type

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ChooseRepaymentCurrencyController @Inject() (
  val jcc: ClaimControllerComponents,
  val chooseCurrencyTypePage: choose_currency_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[Claim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val nextPage: Call = routes.EnterBankAccountDetailsController.show

  final val show: Action[AnyContent] =
    actionReadWriteClaim { implicit claim =>
      if claim.needsCurrencyTypeSelection
      then {
        val form: Form[CurrencyType] =
          Forms.currencyTypeForm.withDefault(claim.answers.currencyType)
        (claim, Ok(chooseCurrencyTypePage(form, routes.ChooseRepaymentCurrencyController.submit)))
      } else {
        (claim, Redirect(nextPage))
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { implicit claim =>
      Forms.currencyTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(
                chooseCurrencyTypePage(formWithErrors, routes.ChooseRepaymentCurrencyController.submit)
              )
            ),
          currencyType =>
            claim
              .submitCurrencyType(currencyType)
              .fold(
                _ => (claim, Redirect(nextPage)),
                updatedClaim => (updatedClaim, Redirect(nextPage))
              )
        )
    }

}
