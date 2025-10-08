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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ProblemWithDeclarationMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_can_continue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_dead_end

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ProblemWithDeclarationController @Inject() (
  val jcc: ClaimControllerComponents,
  override val problemWithDeclarationCanContinuePage: problem_with_declaration_can_continue,
  override val problemWithDeclarationDeadEndPage: problem_with_declaration_dead_end
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleClaimBaseController
    with ProblemWithDeclarationMixin {

  override def removeUnsupportedTaxCodesFromClaim(
    claim: RejectedGoodsSingleClaim
  ): RejectedGoodsSingleClaim =
    claim.removeUnsupportedTaxCodes()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleClaim]] =
    Some(hasMRNAndDisplayDeclaration)

  final override val postAction: Call =
    routes.ProblemWithDeclarationController.submit

  final override val enterAnotherMrnAction: Call =
    routes.EnterMovementReferenceNumberController.show

  final override val checkDeclarationDetailsAction: Call =
    routes.CheckDeclarationDetailsController.show
}
