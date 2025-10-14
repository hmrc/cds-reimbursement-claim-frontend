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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_declaration_details

import scala.concurrent.ExecutionContext

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  val checkDeclarationDetailsPage: check_declaration_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesClaimBaseController {

  private val postAction: Call = routes.CheckDeclarationDetailsController.submit

  import SecuritiesClaim.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] =
    simpleActionReadWriteClaim { claim =>
      val updatedClaim = claim.submitCheckDeclarationDetailsChangeMode(true)
      (
        updatedClaim,
        claim.getLeadDisplayDeclaration
          .fold(Redirect(routes.EnterMovementReferenceNumberController.show))(declaration =>
            Ok(checkDeclarationDetailsPage(declaration, claim.getSecuritiesReclaims, postAction))
          )
      )
    }

  final val submit: Action[AnyContent] =
    simpleActionReadWriteClaim { claim =>
      val updatedClaim = claim.submitCheckDeclarationDetailsChangeMode(false)
      if claim.getSelectedDepositIds.isEmpty then
        (updatedClaim, Redirect(routes.CheckDeclarationDetailsController.show))
      else if claim.userHasSeenCYAPage then (updatedClaim, Redirect(routes.CheckYourAnswersController.show))
      else if claim.getReasonForSecurity.exists(ntas.contains) || claim.getReasonForSecurity.contains(
          MissingPreferenceCertificate
        )
      then (updatedClaim, Redirect(routes.HaveDocumentsReadyController.show))
      else if claim.isSingleSecurity then (updatedClaim, Redirect(routes.ConfirmSingleDepositRepaymentController.show))
      else (updatedClaim, Redirect(routes.ConfirmFullRepaymentController.showFirst))
    }
}
