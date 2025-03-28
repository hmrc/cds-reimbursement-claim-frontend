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
import scala.concurrent.Future

@Singleton
class ChooseExportMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseExportMethodPage: choose_export_method
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val form: Form[List[TemporaryAdmissionMethodOfDisposal]] = chooseExportMethodForm

  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmission(journey) {
      (
        journey,
        Ok(
          chooseExportMethodPage(
            chooseExportMethodForm.withDefault(journey.answers.temporaryAdmissionMethodsOfDisposal),
            routes.ChooseExportMethodController.submit
          )
        )
      )
    }
  }

  def submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmission(journey) {
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                chooseExportMethodPage(
                  formWithErrors,
                  routes.ChooseExportMethodController.submit
                )
              )
            ),
          methodsOfDisposal =>
            journey
              .submitTemporaryAdmissionMethodsOfDisposal(methodsOfDisposal)
              .fold(
                error => {
                  logger.warn(error)
                  (journey, errorHandler.errorResult())
                },
                updatedJourney =>
                  if TemporaryAdmissionMethodOfDisposal.containsExportedMethodsOfDisposal(methodsOfDisposal) then {
                    (updatedJourney, Redirect(routes.EnterExportMovementReferenceNumberController.showFirst))
                  } else {
                    (
                      updatedJourney,
                      if journey.getSecurityDetails.size == 1
                      then Redirect(routes.ChoosePayeeTypeController.show)
                      else Redirect(routes.ConfirmFullRepaymentController.showFirst)
                    )
                  }
              )
        )
    }
  }

  def whenTemporaryAdmission(
    journey: SecuritiesJourney
  )(body: => (SecuritiesJourney, Result))(implicit request: Request[?]): Future[(SecuritiesJourney, Result)] =
    journey.getReasonForSecurity
      .fold((journey, errorHandler.errorResult())) {
        case rfs if ReasonForSecurity.ntas.contains(rfs) => body
        case _                                           => (journey, Redirect(routes.CheckClaimantDetailsController.show))
      }
      .asFuture
}
