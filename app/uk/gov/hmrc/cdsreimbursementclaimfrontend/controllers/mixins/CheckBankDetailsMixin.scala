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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_details_are_correct

trait CheckBankDetailsMixin extends ClaimBaseController {

  val postAction: Call
  def continueRoute(claim: Claim): Call
  val changeBankAccountDetailsRoute: Call
  val enterBankAccountDetailsRoute: Call
  val checkBankDetailsAreCorrectPage: check_bank_details_are_correct
  def isCMA(claim: Claim): Boolean = false

  def modifyClaim(claim: Claim, bankAccountDetails: BankAccountDetails): Either[String, Claim]
  def modifyClaimRemoveBankDetails(claim: Claim): Claim

  final val bankDetailsAreYouSureForm: Form[YesNo] =
    YesOrNoQuestionForm("bank-details")

  final val showWarning: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      claim.answers.bankAccountDetails
        .map { (bankAccountDetails: BankAccountDetails) =>
          modifyClaim(claim, bankAccountDetails)
            .fold(
              _ => (claim, Redirect(continueRoute(claim))),
              claimWithBankDetails =>
                (
                  claimWithBankDetails,
                  Ok(
                    checkBankDetailsAreCorrectPage(
                      bankDetailsAreYouSureForm,
                      bankAccountDetails,
                      isCMA(claim),
                      postAction,
                      changeBankAccountDetailsRoute
                    )
                  )
                )
            )
        }
        .getOrElse {
          (
            claim,
            Redirect(
              if claim.needsBanksAccountDetailsSubmission then enterBankAccountDetailsRoute
              else continueRoute(claim)
            )
          )
        }

    }

  final val submitWarning: Action[AnyContent] =
    simpleActionReadWriteClaim { claim =>
      bankDetailsAreYouSureForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              claim.answers.bankAccountDetails
                .map { bankAccountDetails =>
                  BadRequest(
                    checkBankDetailsAreCorrectPage(
                      formWithErrors,
                      bankAccountDetails.masked,
                      isCMA(claim),
                      postAction,
                      changeBankAccountDetailsRoute
                    )
                  )
                }
                .getOrElse(InternalServerError)
            ),
          answer =>
            answer match {
              case YesNo.Yes => (claim, Redirect(continueRoute(claim)))
              case YesNo.No  => (modifyClaimRemoveBankDetails(claim), Redirect(changeBankAccountDetailsRoute))
            }
        )
    }
}
