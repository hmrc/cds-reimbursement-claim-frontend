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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Indeterminate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Partial
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.Future

object EnterBankAccountDetailsMixin {
  final case class RoutesPack(
    validationErrorPath: Call,
    retryPath: Call,
    successPath: Call,
    submitPath: Call
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
    Ok(
      enterBankAccountDetailsPage(
        enterBankDetailsForm.withDefault(journey.answers.bankAccountDetails),
        isCMA(journey),
        routesPack.submitPath
      )
    ).asFuture
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      implicit journey =>
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
          ),
    fastForwardToCYAEnabled = false
  )

  def validateBankAccountDetails(
    journey: Journey,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[_]): Future[(Journey, Result)] =
    bankAccountReputationService
      .checkBankAccountReputationV2(bankAccountDetails, postCode)
      .fold(
        e => (journey, processCdsError(e, routesPack.validationErrorPath)),
        bankAccountReputation =>
          processBankAccountReputation(journey, bankAccountReputation, bankAccountDetails, routesPack)
      )

  private def processBankAccountReputation(
    journey: Journey,
    bankAccountReputation: BankAccountReputation,
    bankAccountDetails: BankAccountDetails,
    nextPage: EnterBankAccountDetailsMixin.RoutesPack
  ): (Journey, Result) =
    modifyJourney(
      journey,
      bankAccountDetails
        .withMaybeAccountName(bankAccountReputation.nameMatches, bankAccountReputation.accountName.map(_.take(40)))
        .withExistenceVerified(bankAccountReputation.accountExists)
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
            bankAccountReputation match {
              case BankAccountReputation(
                    Yes | Indeterminate,
                    accountExists @ Some(Yes | Indeterminate),
                    _,
                    _,
                    (Some(Yes) | Some(Partial) | None)
                  ) if !journey.isInstanceOf[SecuritiesJourney] || accountExists.contains(Yes) =>
                Redirect(
                  if (journey.userHasSeenCYAPage) checkYourAnswers
                  else nextPage.successPath
                )

              case _ =>
                Redirect(nextPage.validationErrorPath)
            }
          )
      )

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

}
