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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.checkTotalImportDischargedForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndImportDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_total_import_discharged_page

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.reasonForSecurityIsIPROrENU

@Singleton
class CheckTotalImportDischargedController @Inject() (
  val jcc: ClaimControllerComponents,
  checkTotalImportDischargedPage: check_total_import_discharged_page
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {
  private val form: Form[YesNo] = checkTotalImportDischargedForm

  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndImportDeclarationAndRfS
        & declarantOrImporterEoriMatchesUserOrHasBeenVerified
        & reasonForSecurityIsIPROrENU
    )

  // Success: Declaration has been found and ReasonForSecurity is InwardProcessingRelief.
  private val successResultBOD3: Result =
    Redirect(routes.UploadBillOfDischarge3Controller.show)

  // Success: Declaration has been found and ReasonForSecurity is EndUseRelief.
  private val successResultBOD4: Result =
    Redirect(routes.UploadBillOfDischarge4Controller.show)

  def show: Action[AnyContent] = actionReadClaim { _ =>
    Ok(checkTotalImportDischargedPage(form, routes.CheckTotalImportDischargedController.submit))
  }

  def submit: Action[AnyContent] = actionReadClaim { claim =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          BadRequest(
            checkTotalImportDischargedPage(formWithErrors, routes.CheckTotalImportDischargedController.submit)
          ),
        {
          case Yes =>
            if claim.reasonForSecurityIsIPR then successResultBOD3
            else if claim.reasonForSecurityIsENU then successResultBOD4
            else {
              logAndDisplayError(
                "Invalid claim routing",
                s"Reason for security [${claim.getReasonForSecurity}] must be one of [InwardProcessingRelief, EndUseRelief]"
              )
            }
          case No  => Redirect(routes.ClaimInvalidNotExportedAllController.show)
        }
      )

  }
}
