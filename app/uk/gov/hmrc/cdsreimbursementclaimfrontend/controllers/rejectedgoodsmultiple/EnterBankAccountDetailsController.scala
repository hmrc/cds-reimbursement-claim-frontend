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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import cats.data.EitherT
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods.{routes => rejectedGoodsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.{Error => ReputationResponseError, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.BadRequestException

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val claimService: ClaimService,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleJourneyBaseController {

  val formKey: String          = "enter-bank-details"
  private val postAction: Call = routes.EnterBankAccountDetailsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Future.successful {
      val form = enterBankDetailsForm
      Ok(enterBankAccountDetailsPage(form, postAction))
    }
  }

  def handleBadReputation(
    bankAccountDetails: BankAccountDetails,
    reputation: BankAccountReputation
  )(implicit request: Request[_]): Result =
    reputation match {
      case BankAccountReputation(_, _, Some(errorResponse))                                                   =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-details", s"error.${errorResponse.code}"),
            postAction
          )
        )
      case BankAccountReputation(Yes, Some(ReputationResponseError), None)                                    =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-details", "error.account-exists-error"),
            postAction
          )
        )
      case BankAccountReputation(Yes, Some(No), None) | BankAccountReputation(Yes, Some(Indeterminate), None) =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-details", "error.account-does-not-exist"),
            postAction
          )
        )
      case BankAccountReputation(No, _, None) | BankAccountReputation(ReputationResponseError, _, None)       =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-details", "error.moc-check-no"),
            postAction
          )
        )
      case BankAccountReputation(Indeterminate, _, None)                                                      =>
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .withError("enter-bank-details", "error.moc-check-failed"),
            postAction
          )
        )

      case default: BankAccountReputation =>
        logger.info(s"Reached default case - response: [$default]")
        BadRequest(
          enterBankAccountDetailsPage(
            enterBankDetailsForm
              .fill(bankAccountDetails)
              .withError("enter-bank-details", "error.account-does-not-exist"),
            postAction
          )
        )
    }

  def checkBankAccountReputation(
    bankAccountType: BankAccountType,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[_]): EitherT[Future, Error, BankAccountReputation] =
    bankAccountType match {
      case BankAccountType.Personal =>
        claimService.getPersonalAccountReputation(bankAccountDetails, postCode)
      case BankAccountType.Business =>
        claimService.getBusinessAccountReputation(bankAccountDetails)
    }

  def validateBankAccountDetails(
    journey: RejectedGoodsMultipleJourney,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[_]): Future[(RejectedGoodsMultipleJourney, Result)] =
    journey.answers.bankAccountType.fold((journey, Redirect(routes.ChooseBankAccountTypeController.show())).asFuture) {
      bankAccountType =>
        checkBankAccountReputation(bankAccountType, bankAccountDetails, postCode)
          .fold(
            {
              case Error(_, Some(t: BadRequestException), _) =>
                logger.warn("Could not contact bank account service: ", t)
                (journey, Redirect(rejectedGoodsRoutes.ServiceUnavailableController.show()))
              case error                                     =>
                (journey, logAndDisplayError("Could not process bank account details: ")(errorHandler, request)(error))
            },
            {
              case BankAccountReputation(Yes, Some(Yes), None) =>
                journey
                  .submitBankAccountDetails(bankAccountDetails)
                  .fold(
                    error => {
                      logger.warn(s"cannot submit bank account details because of $error")
                      (
                        journey,
                        Redirect(routes.EnterBankAccountDetailsController.show())
                      )
                    },
                    modifiedJourney => (modifiedJourney, Redirect(routes.CheckBankDetailsController.show()))
                  )
              case badReputation                               =>
                (journey, handleBadReputation(bankAccountDetails, badReputation))
            }
          )
    }

  val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      enterBankDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterBankAccountDetailsPage(
                  formWithErrors,
                  postAction
                )
              )
            ).asFuture,
          validateBankAccountDetails(journey, _, None)
        )
    },
    fastForwardToCYAEnabled = false
  )
}
