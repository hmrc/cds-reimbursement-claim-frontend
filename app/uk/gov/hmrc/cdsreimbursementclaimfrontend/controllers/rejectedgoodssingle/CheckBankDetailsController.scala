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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.CheckBankDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_details_are_correct

import scala.concurrent.ExecutionContext

@Singleton
class CheckBankDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  val checkBankDetailsAreCorrectPage: check_bank_details_are_correct
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleClaimBaseController
    with CheckBankDetailsMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final override val postAction: Call =
    routes.CheckBankDetailsController.submitWarning

  final override def continueRoute(claim: Claim): Call =
    if claim.userHasSeenCYAPage then checkYourAnswers
    else routes.ChooseFileTypeController.show

  final override val enterBankAccountDetailsRoute: Call =
    routes.EnterBankAccountDetailsController.show

  final override val changeBankAccountDetailsRoute: Call =
    enterBankAccountDetailsRoute

  final override def modifyClaim(claim: Claim, bankAccountDetails: BankAccountDetails): Either[String, Claim] =
    claim.submitBankAccountDetails(bankAccountDetails)

  final override def modifyClaimRemoveBankDetails(claim: Claim): Claim =
    claim.removeBankAccountDetails()

  final override def isCMA(claim: Claim): Boolean =
    claim.answers.reimbursementMethod.contains(ReimbursementMethod.CurrentMonthAdjustment)

}
