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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterBankAccountDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.ExecutionContext

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val enterBankAccountDetailsPage: enter_bank_account_details,
  val bankAccountReputationService: BankAccountReputationService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController
    with EnterBankAccountDetailsMixin {

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final override def bankAccountType(journey: SecuritiesJourney): Option[BankAccountType] =
    journey.answers.bankAccountType

  final override def submitBankAccountDetails(
    journey: SecuritiesJourney,
    bankAccountDetails: BankAccountDetails
  ): Either[String, SecuritiesJourney] =
    journey.submitBankAccountDetails(bankAccountDetails)

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
