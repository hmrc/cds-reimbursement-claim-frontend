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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import cats.implicits._
import play.api.i18n.Messages
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Indeterminate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.{Error => ReputationResponseError}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait EnterBankAccountDetailsMixin {
  self: JourneyBaseController =>

  val enterBankAccountDetailsPage: enter_bank_account_details
  val bankAccountReputationService: BankAccountReputationService

  def bankAccountType(journey: Journey): Option[BankAccountType]
  def submitBankAccountDetails(journey: Journey, bankAccountDetails: BankAccountDetails): Either[String, Journey]

  final def handleBadReputation(
    bankAccountDetails: BankAccountDetails,
    reputation: BankAccountReputation,
    postAction: Call
  )(implicit
    request: Request[_],
    viewConfig: ViewConfig,
    messages: Messages
  ): Result =
    reputation match {
      case BankAccountReputation(_, _, Some(errorResponse))                                                   =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-account-details", s"error.${errorResponse.code}"),
            postAction
          )
        )
      case BankAccountReputation(Yes, Some(ReputationResponseError), None)                                    =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-account-details", "error.account-exists-error"),
            postAction
          )
        )
      case BankAccountReputation(Yes, Some(No), None) | BankAccountReputation(Yes, Some(Indeterminate), None) =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-account-details", "error.account-does-not-exist"),
            postAction
          )
        )
      case BankAccountReputation(No, _, None) | BankAccountReputation(ReputationResponseError, _, None)       =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-account-details", "error.moc-check-no"),
            postAction
          )
        )
      case BankAccountReputation(Indeterminate, _, None)                                                      =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-account-details", "error.moc-check-failed"),
            postAction
          )
        )

      case default: BankAccountReputation =>
        logger.info(s"Reached default case - response: [$default]")
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-account-details", "error.account-does-not-exist"),
            postAction
          )
        )
    }

  final def processCdsError[E : CdsError](error: E, errorPage: Call)(implicit
    request: Request[_],
    errorHandler: ErrorHandler
  ): Result =
    error match {
      case e @ ServiceUnavailableError(_, _) =>
        logger.warn(s"could not contact bank account service: $e")
        Redirect(errorPage)
      case e                                 =>
        logAndDisplayError("could not process bank account details: ", e)
    }

  case class NextPage(
    errorPath: Call,
    retryPath: Call,
    successPath: Call,
    submitPath: Call,
    getBankAccountTypePath: Call
  )

  private def processBankAccountReputation(
    journey: Journey,
    bankAccountReputation: BankAccountReputation,
    bankAccountDetails: BankAccountDetails,
    nextPage: NextPage
  )(implicit
    request: Request[_],
    messages: Messages
  ): (Journey, Result) = bankAccountReputation match {
    case BankAccountReputation(Yes, Some(Yes), None) =>
      submitBankAccountDetails(journey, bankAccountDetails)
        .fold(
          error => {
            logger.warn(s"cannot submit bank account details because of $error")
            (
              journey,
              Redirect(nextPage.retryPath)
            )
          },
          modifiedJourney =>
            (
              modifiedJourney,
              Redirect(nextPage.successPath)
            )
        )
    case badReputation                               =>
      (journey, handleBadReputation(bankAccountDetails, badReputation, nextPage.submitPath))
  }

  private def getBankAccountType(journey: Journey, getBankAccountTypePage: Call) =
    bankAccountType(journey)
      .toRight((journey, Redirect(getBankAccountTypePage)))

  final def validateBankAccountDetails(
    journey: Journey,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String],
    nextPage: NextPage
  )(implicit
    hc: HeaderCarrier,
    request: Request[_],
    errorHandler: ErrorHandler,
    messages: Messages
  ): Future[(Journey, Result)] =
    getBankAccountType(journey, nextPage.getBankAccountTypePath)
      .fold(
        identity(_).asFuture: Future[(Journey, Result)],
        bankAccountType =>
          bankAccountReputationService
            .checkBankAccountReputation(bankAccountType, bankAccountDetails, postCode)
            .fold(
              e => (journey, processCdsError(e, nextPage.errorPath)),
              bankAccountReputation =>
                processBankAccountReputation(journey, bankAccountReputation, bankAccountDetails, nextPage)
            )
      )
}
