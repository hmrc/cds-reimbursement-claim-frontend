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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterDeclarantEoriNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_declarant_eori_number

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterDeclarantEoriNumberOfDuplicateDeclarationController @Inject() (
  val jcc: ClaimControllerComponents,
  val enterDeclarantEoriNumber: enter_declarant_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleClaimBaseController
    with EnterDeclarantEoriNumberMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleClaim]] =
    Some(
      hasMRNAndDisplayDeclaration &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified &
        needsDuplicateMrnAndDeclaration &
        hasDuplicateMRNAndDisplayDeclaration
    )

  final override val postAction: Call =
    routes.EnterDeclarantEoriNumberOfDuplicateDeclarationController.submit

  final override val continueAction: Call =
    routes.CheckDuplicateDeclarationDetailsController.show

  final override val whenEoriInputNotRequiredAction: Call =
    routes.EnterAdditionalDetailsController.show

  final override def needsEoriSubmission(claim: Claim): Boolean =
    claim.needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration

  final override def getEoriNumberAnswer(claim: Claim): Option[Eori] =
    None

  final override def modifyClaim(claim: Claim, eori: Eori): Either[String, Claim] =
    claim.checkDeclarantEoriNumberWithDuplicateDeclaration(eori)

}
