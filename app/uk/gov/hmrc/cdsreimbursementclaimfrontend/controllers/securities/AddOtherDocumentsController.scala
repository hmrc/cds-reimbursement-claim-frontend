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

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.addOtherDocumentsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.needsAddOtherDocuments
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.add_other_documents_page

import scala.concurrent.ExecutionContext

@Singleton
class AddOtherDocumentsController @Inject() (
  val jcc: JourneyControllerComponents,
  addOtherDocumentsPage: add_other_documents_page
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {
  private val form: Form[YesNo] = addOtherDocumentsForm

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS
        & declarantOrImporterEoriMatchesUserOrHasBeenVerified
        & needsAddOtherDocuments
    )

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(addOtherDocumentsPage(form, routes.AddOtherDocumentsController.submit)).asFuture
  }

  def submit: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          BadRequest(
            addOtherDocumentsPage(formWithErrors, routes.AddOtherDocumentsController.submit)
          ).asFuture,
        {
          case Yes => Redirect(routes.ChooseFileTypeController.show).asFuture
          case No  =>
            Redirect(
              if journey.reasonForSecurityIsNidac
              then routes.EnterAdditionalDetailsController.show
              else routes.ChoosePayeeTypeController.show
            ).asFuture
        }
      )

  }
}
