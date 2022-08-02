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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods.{routes => rejectedGoodsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val bankAccountReputationService: BankAccountReputationService,
  enterBankAccountDetailsPage: enter_bank_account_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController {

  val formKey: String          = "enter-bank-account-details"
  private val postAction: Call = routes.EnterBankAccountDetailsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Future.successful {
      val form = enterBankDetailsForm
      Ok(enterBankAccountDetailsPage(form, postAction))
    }
  }

  private def processCdsError[T : CdsError](error: T)(implicit request: Request[_]): Result =
    error match {
      case e @ ServiceUnavailableError(_, _) =>
        logger.warn(s"could not contact bank account service: $e")
        Redirect(rejectedGoodsRoutes.ServiceUnavailableController.show())
      case e                                 =>
        logAndDisplayError("could not process bank account details: ", e)
    }

  def handleBankAccountReputation[E : CdsError](
    bankAccountDetails: BankAccountDetails,
    reputation: EitherT[Future, E, BankAccountReputation]
  )(implicit request: Request[_], journey: RejectedGoodsSingleJourney): Future[(RejectedGoodsSingleJourney, Result)] =
    reputation.fold(
      e => (journey, processCdsError(e)),
      {
        case BankAccountReputation(Yes, Some(Yes), None)                                  =>
          journey
            .submitBankAccountDetails(bankAccountDetails)
            .fold(
              errors => {
                logger.error(s"Unable to get bank account details - $errors")
                (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
              },
              updatedJourney => (updatedJourney, Redirect(routes.CheckBankDetailsController.show()))
            )
        case BankAccountReputation(_, _, Some(errorResponse))                             =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-account-details", s"error.${errorResponse.code}")
          (journey, BadRequest(enterBankAccountDetailsPage(form, postAction)))
        case BankAccountReputation(No, _, None)                                           =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-account-details", "error.moc-check-no")
          (journey, BadRequest(enterBankAccountDetailsPage(form, postAction)))
        case BankAccountReputation(sortCodeResponse, _, None) if sortCodeResponse =!= Yes =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-account-details", "error.moc-check-failed")
          (journey, BadRequest(enterBankAccountDetailsPage(form, postAction)))
        case BankAccountReputation(_, Some(ReputationResponse.Error), None)               =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-account-details", "error.account-exists-error")
          (journey, BadRequest(enterBankAccountDetailsPage(form, postAction)))
        case BankAccountReputation(_, _, None)                                            =>
          val form = enterBankDetailsForm
            .fill(bankAccountDetails)
            .withError("enter-bank-account-details", "error.account-does-not-exist")
          (journey, BadRequest(enterBankAccountDetailsPage(form, postAction)))
      }
    )

  def validateBankAccountDetails(
    bankAccountType: Option[BankAccountType],
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit request: Request[_], journey: RejectedGoodsSingleJourney): Future[(RejectedGoodsSingleJourney, Result)] =
    bankAccountType match {
      case Some(BankAccountType.Personal) =>
        handleBankAccountReputation(
          bankAccountDetails,
          bankAccountReputationService.getPersonalAccountReputation(bankAccountDetails, postCode)
        )
      case Some(BankAccountType.Business) =>
        handleBankAccountReputation(
          bankAccountDetails,
          bankAccountReputationService.getBusinessAccountReputation(bankAccountDetails)
        )
      case _                              =>
        (journey, Redirect(routes.ChooseBankAccountTypeController.show())).asFuture
    }

  val submit: Action[AnyContent] = actionReadWriteJourney(
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
                  postAction
                )
              )
            ).asFuture,
          validateBankAccountDetails(journey.answers.bankAccountType, _, None)
        )
    },
    fastForwardToCYAEnabled = false
  )
}
