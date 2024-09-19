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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterBankAccountDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val claimService: ClaimService,
  val bankAccountReputationService: BankAccountReputationService,
  val enterBankAccountDetailsPage: enter_bank_account_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleJourneyBaseController
    with EnterBankAccountDetailsMixin {

  final override def modifyJourney(
    journey: Journey,
    bankAccountDetails: BankAccountDetails
  ): Either[String, Journey] =
    journey.submitBankAccountDetails(bankAccountDetails)

  override val routesPack = EnterBankAccountDetailsController.routesPack

}

object EnterBankAccountDetailsController {
  val routesPack = EnterBankAccountDetailsMixin.RoutesPack(
    validationErrorPath = routes.CheckBankDetailsController.showWarning,
    retryPath = routes.EnterBankAccountDetailsController.show,
    successPath = routes.CheckBankDetailsController.show,
    submitPath = routes.EnterBankAccountDetailsController.submit
  )
}
