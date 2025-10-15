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
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.chooseExportMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndImportDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_export_method

import scala.concurrent.ExecutionContext

@Singleton
class ChooseExportMethodController @Inject() (
  val jcc: ClaimControllerComponents,
  chooseExportMethodPage: choose_export_method
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {

  private val form: Form[List[TemporaryAdmissionMethodOfDisposal]] = chooseExportMethodForm

  override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndImportDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val show: Action[AnyContent] = actionReadWriteClaim { claim =>
    whenTemporaryAdmission(claim) {
      (
        claim,
        Ok(
          chooseExportMethodPage(
            chooseExportMethodForm.withDefault(claim.answers.temporaryAdmissionMethodsOfDisposal),
            routes.ChooseExportMethodController.submit
          )
        )
      )
    }
  }

  val submit: Action[AnyContent] = actionReadWriteClaim { claim =>
    whenTemporaryAdmission(claim) {
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(
                chooseExportMethodPage(
                  formWithErrors,
                  routes.ChooseExportMethodController.submit
                )
              )
            ),
          methodsOfDisposal =>
            claim
              .submitTemporaryAdmissionMethodsOfDisposal(methodsOfDisposal)
              .fold(
                error => {
                  logger.warn(error)
                  (claim, errorHandler.errorResult())
                },
                updatedClaim =>
                  if TemporaryAdmissionMethodOfDisposal.containsExportedMethodsOfDisposal(methodsOfDisposal) then {
                    (updatedClaim, Redirect(routes.EnterExportMovementReferenceNumberController.showFirst))
                  } else {
                    (
                      updatedClaim,
                      if claim.isSingleSecurity
                      then Redirect(routes.ChoosePayeeTypeController.show)
                      else Redirect(routes.ConfirmFullRepaymentController.showFirst)
                    )
                  }
              )
        )
    }
  }

  def whenTemporaryAdmission(
    claim: SecuritiesClaim
  )(body: => (SecuritiesClaim, Result))(implicit request: Request[?]): (SecuritiesClaim, Result) =
    claim.getReasonForSecurity
      .fold((claim, errorHandler.errorResult())) {
        case rfs if ReasonForSecurity.ntas.contains(rfs) => body
        case _                                           => (claim, Redirect(routes.CheckClaimantDetailsController.show))
      }

}
