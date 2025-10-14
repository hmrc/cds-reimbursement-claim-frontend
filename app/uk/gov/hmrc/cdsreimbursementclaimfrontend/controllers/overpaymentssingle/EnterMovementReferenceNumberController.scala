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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_movement_reference_number
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.subsidy_waiver_error

import scala.concurrent.ExecutionContext

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: ClaimControllerComponents,
  val claimService: ClaimService,
  val xiEoriConnector: XiEoriConnector,
  val featureSwitchService: FeatureSwitchService,
  enterMovementReferenceNumberPage: enter_movement_reference_number,
  subsidyWaiverPage: subsidy_waiver_error
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends OverpaymentsSingleClaimBaseController
    with EnterMovementReferenceNumberMixin {

  override val problemWithMrnCall: MRN => Call = routes.ProblemWithMrnController.show

  override def form(claim: Claim): Form[MRN] =
    Forms.movementReferenceNumberForm

  override def getMovementReferenceNumber(claim: Claim): Option[MRN] =
    claim.getLeadMovementReferenceNumber

  override def viewTemplate: Form[MRN] => Request[?] ?=> HtmlFormat.Appendable =
    form =>
      enterMovementReferenceNumberPage(
        form,
        "single",
        None,
        routes.EnterMovementReferenceNumberController.submit
      )

  override def subsidyWaiverErrorPage: (MRN, Boolean) => Request[?] ?=> HtmlFormat.Appendable =
    (mrn, isOnlySubsidies) =>
      subsidyWaiverPage(
        "C285",
        "single",
        if isOnlySubsidies then "full" else "part",
        mrn,
        routes.EnterMovementReferenceNumberController.submitWithoutSubsidies,
        viewConfig.legacyC285FormUrl,
        routes.EnterMovementReferenceNumberController.show
      )

  override def modifyClaim(claim: Claim, mrn: MRN, declaration: DisplayDeclaration): Either[String, Claim] =
    claim.submitMovementReferenceNumberAndDeclaration(mrn, declaration)

  override def modifyClaim(claim: Claim, userXiEori: UserXiEori): Claim =
    claim.submitUserXiEori(userXiEori)

  override def afterSuccessfullSubmit(claim: OverpaymentsSingleClaim): Result =
    if claim.containsUnsupportedTaxCode then {
      Redirect(routes.ProblemWithDeclarationController.show)
    } else if claim.needsDeclarantAndConsigneeEoriSubmission then {
      Redirect(routes.EnterImporterEoriNumberController.show)
    } else {
      Redirect(routes.CheckDeclarationDetailsController.show)
    }

}
