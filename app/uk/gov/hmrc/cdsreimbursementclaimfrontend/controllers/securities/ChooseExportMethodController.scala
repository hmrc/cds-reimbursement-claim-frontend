/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.chooseExportMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_export_method

import scala.concurrent.ExecutionContext

@Singleton
class ChooseExportMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseExportMethodPage: choose_export_method
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val form: Form[Option[TemporaryAdmissionMethodOfDisposal]] = chooseExportMethodForm

  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    journey.getReasonForSecurity
      .fold((journey, errorHandler.errorResult())) {
        case rfs if ReasonForSecurity.temporaryAdmissions.contains(rfs) =>
          (
            journey,
            Ok(chooseExportMethodPage(chooseExportMethodForm, routes.ChooseExportMethodController.submit()))
          )
        case _                                                          => (journey, Redirect(routes.CheckClaimantDetailsController.show()))
      }
      .asFuture
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form.bindFromRequest
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              chooseExportMethodPage(
                formWithErrors,
                routes.ChooseExportMethodController.submit()
              )
            )
          ),
        {
          case None                   =>
            val e = "no value was submitted for TemporaryAdmissionMethodOfDisposal, but there were no form errors"
            routeTemporaryAdmissionWithError(journey, e)
          case Some(methodOfDisposal) =>
            journey
              .submitTemporaryAdmissionMethodOfDisposal(methodOfDisposal)
              .fold(
                e => routeTemporaryAdmissionWithError(journey, e),
                updatedJourney => routeTemporaryAdmission(updatedJourney, methodOfDisposal)
              )
        }
      )
      .asFuture
  }

  def routeTemporaryAdmissionWithError(journey: SecuritiesJourney, error: String)(implicit
    request: Request[_]
  ): (SecuritiesJourney, Result) = journey.getReasonForSecurity match {
    case Some(value) if !ReasonForSecurity.temporaryAdmissions.contains(value) =>
      (journey, Redirect(routes.CheckClaimantDetailsController.show()))
    case None                                                                  =>
      logger.warn(error)
      (journey, errorHandler.errorResult())
  }

  def routeTemporaryAdmission(updatedJourney: SecuritiesJourney, methodOfDisposal: TemporaryAdmissionMethodOfDisposal)(
    implicit request: Request[_]
  ): (SecuritiesJourney, Result) =
    updatedJourney.getReasonForSecurity match {
      case Some(value: ReasonForSecurity) if ReasonForSecurity.temporaryAdmissions.contains(value) =>
        methodOfDisposal match {
          case TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment    =>
            (updatedJourney, Redirect(routes.EnterExportMovementReferenceNumberController.show()))
          case TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments =>
            (updatedJourney, Redirect(routes.EnterExportMovementReferenceNumberMultipleController.show()))
          case _                                                              =>
            (updatedJourney, Redirect(routes.CheckClaimantDetailsController.show()))
        }
      case None                                                                                    => (updatedJourney, Redirect(routes.CheckClaimantDetailsController.show()))
    }
}
