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
import play.api.data.FormError
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.EnterExportMovementReferenceNumberController.enterExportMovementReferenceNumberKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.EnterExportMovementReferenceNumberController.exportMovementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number

import scala.concurrent.ExecutionContext

@Singleton
class EnterExportMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterExportMovementReferenceNumberPage: enter_export_movement_reference_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )
  def show: Action[AnyContent]                                               = actionReadJourney { implicit request => journey =>
    Ok(
      enterExportMovementReferenceNumberPage(
        exportMovementReferenceNumberForm,
        routes.EnterExportMovementReferenceNumberController.submit()
      )
    ).asFuture
  }

  def submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    exportMovementReferenceNumberForm.bindFromRequest
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              enterExportMovementReferenceNumberPage(
                formWithErrors.copy(data = Map.empty),
                routes.EnterExportMovementReferenceNumberController.submit()
              )
            )
          ).asFuture,
        mrn =>
          claimService
            .getDisplayDeclaration(mrn)
            .fold(
              _ => submitMrnAndContinue(mrn, journey),
              {
                case None    => submitMrnAndContinue(mrn, journey)
                case Some(_) =>
                  (
                    journey,
                    BadRequest(
                      enterExportMovementReferenceNumberPage(
                        exportMovementReferenceNumberForm.copy(
                          data = Map.empty,
                          errors = List(FormError(enterExportMovementReferenceNumberKey, "securities.error.import"))
                        ),
                        routes.EnterExportMovementReferenceNumberController.submit()
                      )
                    )
                  )
              }
            )
      )
  }

  private def submitMrnAndContinue(mrn: MRN, journey: SecuritiesJourney): (SecuritiesJourney, Result) =
    journey
      .submitExportMovementReferenceNumber(mrn)
      .fold(
        _ => (journey, Redirect(routes.CheckClaimantDetailsController.show())),
        (_, Redirect(routes.CheckClaimantDetailsController.show()))
      )
}

object EnterExportMovementReferenceNumberController {
  val enterExportMovementReferenceNumberKey: String = "enter-export-movement-reference-number"
  val exportMovementReferenceNumberForm: Form[MRN]  =
    Form(
      mapping(
        enterExportMovementReferenceNumberKey ->
          nonEmptyText
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )
}
