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

import cats.implicits.*
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Indeterminate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Partial
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.Future

object EnterBankAccountDetailsMixin {
  final case class RoutesPack[Claim](
    validationErrorPath: Call,
    retryPath: Call,
    successPath: Claim => Call,
    submitPath: Call
  )
}

trait EnterBankAccountDetailsMixin extends ClaimBaseController {

  val enterBankAccountDetailsPage: enter_bank_account_details
  val bankAccountReputationService: BankAccountReputationService
  val routesPack: EnterBankAccountDetailsMixin.RoutesPack[Claim]
  def isCMA(claim: Claim): Boolean = false

  implicit val errorHandler: ErrorHandler

  def modifyClaim(claim: Claim, bankAccountDetails: BankAccountDetails): Either[String, Claim]

  final val show: Action[AnyContent] = actionReadClaim { claim =>
    Ok(
      enterBankAccountDetailsPage(
        enterBankDetailsForm.withDefault(claim.answers.bankAccountDetails),
        isCMA(claim),
        routesPack.submitPath
      )
    )
  }

  final val submit: Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      implicit claim =>
        enterBankDetailsForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                claim,
                BadRequest(
                  enterBankAccountDetailsPage(
                    formWithErrors,
                    isCMA(claim),
                    routesPack.submitPath
                  )
                )
              ),
            validateBankAccountDetails(claim, _, None)
          ),
    fastForwardToCYAEnabled = false
  )

  def validateBankAccountDetails(
    claim: Claim,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[?]): Future[(Claim, Result)] =
    bankAccountReputationService
      .checkBankAccountReputationV2(bankAccountDetails, postCode)
      .fold(
        e => (claim, processCdsError(e, routesPack.validationErrorPath)),
        bankAccountReputation =>
          processBankAccountReputation(claim, bankAccountReputation, bankAccountDetails, routesPack)
      )

  private def processBankAccountReputation(
    claim: Claim,
    bankAccountReputation: BankAccountReputation,
    bankAccountDetails: BankAccountDetails,
    nextPage: EnterBankAccountDetailsMixin.RoutesPack[Claim]
  ): (Claim, Result) =
    modifyClaim(
      claim,
      bankAccountDetails
        .withMaybeAccountName(bankAccountReputation.nameMatches, bankAccountReputation.accountName.map(_.take(40)))
        .withExistenceVerified(bankAccountReputation.accountExists)
    )
      .fold(
        error => {
          logger.warn(s"cannot submit bank account details because of $error")
          (
            claim,
            Redirect(nextPage.retryPath)
          )
        },
        modifiedClaim =>
          (
            modifiedClaim,
            bankAccountReputation match {
              case BankAccountReputation(
                    Yes | Indeterminate,
                    accountExists @ Some(Yes | Indeterminate),
                    _,
                    _,
                    (Some(Yes) | Some(Partial) | None)
                  ) if !claim.isInstanceOf[SecuritiesClaim] || accountExists.contains(Yes) =>
                Redirect(
                  if claim.userHasSeenCYAPage
                  then checkYourAnswers
                  else nextPage.successPath(claim)
                )

              case _ =>
                Redirect(nextPage.validationErrorPath)
            }
          )
      )

  private def processCdsError[E : CdsError](error: E, errorPage: Call)(implicit
    request: Request[?],
    errorHandler: ErrorHandler
  ): Result =
    error match {
      case e @ ServiceUnavailableError(_, _) =>
        logger.warn(s"could not contact bank account service: $e")
        Redirect(errorPage)
      case e                                 =>
        logAndDisplayError("could not process bank account details: ", e)
    }

}
