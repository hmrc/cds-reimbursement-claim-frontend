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

import cats.implicits._
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.{Error => ReputationResponseError}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Indeterminate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Partial
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.Future

object EnterBankAccountDetailsMixin {
  final case class RoutesPack(
    errorPath: Call,
    retryPath: Call,
    successPath: Call,
    submitPath: Call,
    getBankAccountTypePath: Call
  )
}

trait EnterBankAccountDetailsMixin extends JourneyBaseController {

  val enterBankAccountDetailsPage: enter_bank_account_details
  val bankAccountReputationService: BankAccountReputationService
  val routesPack: EnterBankAccountDetailsMixin.RoutesPack
  def isCMA(journey: Journey): Boolean = false

  implicit val errorHandler: ErrorHandler

  def modifyJourney(journey: Journey, bankAccountDetails: BankAccountDetails): Either[String, Journey]

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(enterBankAccountDetailsPage(enterBankDetailsForm, isCMA(journey), routesPack.submitPath)).asFuture
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => implicit journey =>
      enterBankDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterBankAccountDetailsPage(
                  formWithErrors,
                  isCMA(journey),
                  routesPack.submitPath
                )
              )
            ).asFuture,
          validateBankAccountDetails(journey, _, None)
        )
    },
    fastForwardToCYAEnabled = false
  )

  def handleBadReputation(
    bankAccountDetails: BankAccountDetails,
    reputation: BankAccountReputation,
    isCMA: Boolean
  )(implicit request: Request[_]): Result =
    reputation match {
      case BankAccountReputation(_, _, Some(errorResponse), _, _)                =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-account-details", s"error.${errorResponse.code}"),
            isCMA,
            routesPack.submitPath
          )
        )
      case BankAccountReputation(Yes, Some(ReputationResponseError), None, _, _) =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-account-details", "error.account-exists-error"),
            isCMA,
            routesPack.submitPath
          )
        )
      case BankAccountReputation(Yes, Some(No), None, _, _) |
          BankAccountReputation(Yes, Some(Indeterminate), None, _, _) =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-account-details", "error.account-does-not-exist"),
            isCMA,
            routesPack.submitPath
          )
        )
      case BankAccountReputation(No, _, None, _, _)                              =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-account-details", "error.moc-check-no"),
            isCMA,
            routesPack.submitPath
          )
        )
      case BankAccountReputation(_, _, None, _, _)                               =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-account-details", "error.moc-check-failed"),
            isCMA,
            routesPack.submitPath
          )
        )

      case default: BankAccountReputation =>
        logger.info(s"Reached default case - response: [$default]")
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-account-details", "error.account-does-not-exist"),
            isCMA,
            routesPack.submitPath
          )
        )
    }

  private def processCdsError[E : CdsError](error: E, errorPage: Call)(implicit
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

  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  private def processBankAccountReputation(
    journey: Journey,
    bankAccountReputation: BankAccountReputation,
    bankAccountDetails: BankAccountDetails,
    nextPage: EnterBankAccountDetailsMixin.RoutesPack
  )(implicit request: Request[_]): (Journey, Result) =
    bankAccountReputation match {
      case BankAccountReputation(
            Yes | Indeterminate,
            accountExists @ Some(Yes | Indeterminate),
            _,
            accountNameOpt,
            nameMatchesOpt @ (Some(Yes) | Some(Partial) | None)
          ) if !journey.isInstanceOf[SecuritiesJourney] || accountExists.contains(Yes) =>
        modifyJourney(
          journey,
          bankAccountDetails
            .withMaybeAccountName(nameMatchesOpt, accountNameOpt)
            .withExistenceVerified(accountExists.contains(ReputationResponse.Yes))
        )
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
      case badReputation =>
        (journey, handleBadReputation(bankAccountDetails, badReputation, isCMA(journey)))
    }

  private def getBankAccountType(journey: Journey, getBankAccountTypePage: Call) =
    journey.answers.bankAccountType
      .toRight((journey, Redirect(getBankAccountTypePage)))

  def validateBankAccountDetails(
    journey: Journey,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[_]): Future[(Journey, Result)] =
    getBankAccountType(journey, routesPack.getBankAccountTypePath)
      .fold(
        identity(_).asFuture: Future[(Journey, Result)],
        bankAccountType =>
          bankAccountReputationService
            .checkBankAccountReputation(bankAccountType, bankAccountDetails, postCode)
            .fold(
              e => (journey, processCdsError(e, routesPack.errorPath)),
              bankAccountReputation =>
                processBankAccountReputation(journey, bankAccountReputation, bankAccountDetails, routesPack)
            )
      )
}
