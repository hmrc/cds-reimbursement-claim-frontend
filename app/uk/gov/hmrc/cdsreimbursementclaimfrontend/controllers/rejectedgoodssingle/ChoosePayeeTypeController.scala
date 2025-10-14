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
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ChoosePayeeTypeMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_payee_type

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ChoosePayeeTypeController @Inject() (
  val jcc: ClaimControllerComponents,
  val choosePayeeTypePage: choose_payee_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleClaimBaseController
    with ChoosePayeeTypeMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[Claim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final override def modifyClaim(claim: Claim, payeeType: PayeeType): Either[String, Claim] =
    claim.submitPayeeType(payeeType)

  final val postAction: Call = routes.ChoosePayeeTypeController.submit

  final def nextPage(claim: Claim): Call =
    if claim.isAllSelectedDutiesAreCMAEligible then routes.ChooseRepaymentMethodController.show
    else routes.EnterBankAccountDetailsController.show

}
