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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.data.EitherT
import javax.inject.Inject
import javax.inject.Singleton
import cats.implicits._
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val claimService: ClaimService,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController {

  val formKey: String          = "enter-bank-details"
  private val postAction: Call = routes.EnterBankAccountDetailsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Future.successful {
      val form = enterBankDetailsForm
      Ok(enterBankAccountDetailsPage(form, postAction))
    }
  }

  def handleBankAccountReputation(
    bankAccountDetails: BankAccountDetails,
    reputation: EitherT[Future, Error, BankAccountReputation]
  )(implicit request: Request[_]): Future[Result] =
    reputation.fold(
      {
        case error if error.responseStatus === Some(BAD_REQUEST) => Redirect(routes.ServiceUnavailableController.show())
        case error => logAndDisplayError("Could not process bank account details: ")(errorHandler, request)(error)
      },
      {
        case BankAccountReputation(Yes, Some(Yes), None)                                  =>
          Redirect(routes.CheckBankDetailsController.show())
        case BankAccountReputation(_, _, Some(errorResponse))                             =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-details", s"error.${errorResponse.code}")
          BadRequest(enterBankAccountDetailsPage(form, postAction))
        case BankAccountReputation(No, _, None)                                           =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-details", "error.moc-check-no")
          BadRequest(enterBankAccountDetailsPage(form, postAction))
        case BankAccountReputation(sortCodeResponse, _, None) if sortCodeResponse =!= Yes =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-details", "error.moc-check-failed")
          BadRequest(enterBankAccountDetailsPage(form, postAction))
        case BankAccountReputation(_, Some(ReputationResponse.Error), None)               =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-details", "error.account-exists-error")
          BadRequest(enterBankAccountDetailsPage(form, postAction))
        case BankAccountReputation(_, _, None)                                            =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-details", "error.account-does-not-exist")
          BadRequest(enterBankAccountDetailsPage(form, postAction))
      }
    )

  def validateBankAccountDetails(
    bankAccountType: Option[BankAccountType],
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[_]): Future[Result] =
    bankAccountType match {
      case Some(BankAccountType.Personal) =>
        handleBankAccountReputation(
          bankAccountDetails,
          claimService.getPersonalAccountReputation(bankAccountDetails, postCode)
        )
      case Some(BankAccountType.Business) =>
        handleBankAccountReputation(
          bankAccountDetails,
          claimService.getBusinessAccountReputation(bankAccountDetails)
        )
      case _                              =>
        Redirect(routes.ChooseBankAccountTypeController.show()).asFuture
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
          bankAccountDetails =>
            journey
              .submitBankAccountDetails(bankAccountDetails)
              .fold(
                errors => {
                  logger.error(s"unable to get bank account details - $errors")
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible())).asFuture
                },
                updatedJourney =>
                  validateBankAccountDetails(journey.answers.bankAccountType, bankAccountDetails, None).map(result =>
                    (
                      updatedJourney,
                      result
                    )
                  )
              )
        )
    },
    fastForwardToCYAEnabled = false
  )
}
