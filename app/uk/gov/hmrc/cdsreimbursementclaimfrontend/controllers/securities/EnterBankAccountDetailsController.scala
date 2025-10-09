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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterBankAccountDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details

import scala.concurrent.ExecutionContext

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  val enterBankAccountDetailsPage: enter_bank_account_details,
  val bankAccountReputationService: BankAccountReputationService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends SecuritiesClaimBaseController
    with EnterBankAccountDetailsMixin {

  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final override def modifyClaim(
    claim: SecuritiesClaim,
    bankAccountDetails: BankAccountDetails
  ): Either[String, SecuritiesClaim] =
    claim.submitBankAccountDetails(bankAccountDetails)

  override val routesPack = EnterBankAccountDetailsController.routesPack

}

object EnterBankAccountDetailsController {

  val routesPack = EnterBankAccountDetailsMixin.RoutesPack(
    validationErrorPath = routes.CheckBankDetailsController.showWarning,
    retryPath = routes.EnterBankAccountDetailsController.show,
    submitPath = routes.EnterBankAccountDetailsController.submit,
    successPath = (claim: SecuritiesClaim) =>
      if claim.reasonForSecurityIsIPROrENU
      then routes.EnterAdditionalDetailsController.show
      else if claim.reasonForSecurityIsNidac
      then routes.UploadProofOfOriginController.show
      else routes.ChooseFileTypeController.show
  )

}
