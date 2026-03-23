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

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.{declarantOrImporterEoriMatchesUserOrHasBeenVerified, hasMRNAndImportDeclarationAndRfS, needsAddOtherDocuments}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.addOtherDocumentsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.add_other_documents_page

import scala.concurrent.ExecutionContext

@Singleton
class AddOtherDocumentsController @Inject() (
  val jcc: ClaimControllerComponents,
  addOtherDocumentsPage: add_other_documents_page
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {
  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndImportDeclarationAndRfS
        & declarantOrImporterEoriMatchesUserOrHasBeenVerified
        & needsAddOtherDocuments
    )
  val show: Action[AnyContent] = actionReadClaim { claim =>
    Ok(addOtherDocumentsPage(form, routes.AddOtherDocumentsController.submit))
  }
  val submit: Action[AnyContent] = actionReadClaim { claim =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          BadRequest(
            addOtherDocumentsPage(formWithErrors, routes.AddOtherDocumentsController.submit)
          ),
        {
          case Yes => Redirect(routes.ChooseFileTypeController.show)
          case No  =>
            Redirect(
              if claim.reasonForSecurityIsNidac
              then routes.EnterAdditionalDetailsController.show
              else routes.ChoosePayeeTypeController.show
            )
        }
      )

  }
  private val form: Form[YesNo] = addOtherDocumentsForm
}
