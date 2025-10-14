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
import play.api.data.Form
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.enter_duplicate_movement_reference_number

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterDuplicateMovementReferenceNumberController @Inject() (
  val jcc: ClaimControllerComponents,
  val claimService: ClaimService,
  val xiEoriConnector: XiEoriConnector,
  val featureSwitchService: FeatureSwitchService,
  enterDuplicateMovementReferenceNumberPage: enter_duplicate_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends OverpaymentsSingleClaimBaseController
    with EnterMovementReferenceNumberMixin {

  override val shouldValidateDeclaration: Boolean = false

  override val problemWithMrnCall: MRN => Call = routes.ProblemWithMrnController.show

  override val formKey = "enter-duplicate-movement-reference-number"

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleClaim]] =
    Some(
      hasMRNAndDisplayDeclaration &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified &
        needsDuplicateMrnAndDeclaration
    )

  override def form(claim: Claim): Form[MRN] =
    claim.answers.movementReferenceNumber
      .fold(Forms.enterDuplicateMrnWithNoCheck)(Forms.enterDuplicateMrnCheckingAgainst)

  override def getMovementReferenceNumber(claim: Claim): Option[MRN] =
    claim.answers.duplicateDeclaration.map(_.movementReferenceNumber)

  override def viewTemplate: Form[MRN] => Request[?] ?=> HtmlFormat.Appendable =
    form =>
      enterDuplicateMovementReferenceNumberPage(
        form,
        Some("overpayments.single"),
        routes.EnterDuplicateMovementReferenceNumberController.submit
      )

  override def subsidyWaiverErrorPage: (MRN, Boolean) => Request[?] ?=> HtmlFormat.Appendable = ???

  override def modifyClaim(claim: Claim, mrn: MRN, declaration: DisplayDeclaration): Either[String, Claim] =
    claim.submitDuplicateMovementReferenceNumberAndDeclaration(mrn, declaration)

  override def modifyClaim(claim: Claim, userXiEori: UserXiEori): Claim =
    claim.submitUserXiEori(userXiEori)

  override def needsUserXiEoriSubmission(claim: Claim): Boolean =
    claim.needsUserXiEoriSubmissionForDuplicateDeclaration

  override def afterSuccessfullSubmit(claim: OverpaymentsSingleClaim): Result =
    if claim.needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration then {
      Redirect(routes.EnterImporterEoriNumberOfDuplicateDeclaration.show)
    } else {
      Redirect(routes.CheckDuplicateDeclarationDetailsController.show)
    }
}
