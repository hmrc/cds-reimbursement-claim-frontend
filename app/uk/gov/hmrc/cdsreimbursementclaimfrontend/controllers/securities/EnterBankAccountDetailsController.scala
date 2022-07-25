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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import shapeless.syntax.std.tuple.productTupleOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterBankAccountDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  implicit val enterBankAccountDetailsPage: enter_bank_account_details,
  implicit val bankAccountReputationService: BankAccountReputationService
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController
    with EnterBankAccountDetailsMixin
    with Logging {

  import JourneyWithSubmitBankAccount._
  implicit def securitiesJourneyWithBankAccount(implicit
    journey: SecuritiesJourney
  ): JourneyWithBankAccount[SecuritiesJourney] =
    instance(journey.answers.bankAccountType, journey.submitBankAccountDetails)

  private val nextPage = NextPage(
    errorPath = commonRoutes.ServiceUnavailableController.show(),
    retryPath = routes.EnterBankAccountDetailsController.show(),
    successPath = routes.CheckBankDetailsController.show(),
    submitPath = routes.EnterBankAccountDetailsController.submit(),
    getBankAccountTypePath = routes.ChooseBankAccountTypeController.show()
  )

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(enterBankAccountDetailsPage(enterBankDetailsForm, routes.EnterBankAccountDetailsController.submit())).asFuture
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
                  nextPage.submitPath
                )
              )
            ).asFuture,
          validateBankAccountDetails(journey, _, None, nextPage)
        )
    },
    fastForwardToCYAEnabled = false
  )
}
