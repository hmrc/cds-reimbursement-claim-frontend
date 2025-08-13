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

import cats.implicits.catsSyntaxEq
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_export_movement_reference_numbers
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

@Singleton
class CheckExportMovementReferenceNumbersController @Inject() (
  val jcc: JourneyControllerComponents,
  checkExportMovementReferenceNumbersPage: check_export_movement_reference_numbers
)(implicit val ec: ExecutionContext, errorHandler: ErrorHandler, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def nextStepInJourney(journey: SecuritiesJourney)(using HeaderCarrier) =
    if journey.isSingleSecurity
    then routes.ChoosePayeeTypeController.show
    else routes.ConfirmFullRepaymentController.showFirst

  private val checkExportMovementReferenceNumbersKey: String = "check-export-movement-reference-numbers"

  private val checkExportMovementReferenceNumbersForm: Form[YesNo] =
    YesOrNoQuestionForm(
      checkExportMovementReferenceNumbersKey
    )

  val show: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) { exportMRNs =>
      if exportMRNs.isEmpty
      then (journey, Redirect(routes.EnterExportMovementReferenceNumberController.showFirst))
      else
        (
          journey,
          Ok(
            checkExportMovementReferenceNumbersPage(
              exportMRNs,
              checkExportMovementReferenceNumbersForm,
              routes.CheckExportMovementReferenceNumbersController.submit,
              routes.EnterExportMovementReferenceNumberController.showNext,
              routes.CheckExportMovementReferenceNumbersController.delete
            )
          )
        )
    }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      journey =>
        whenTemporaryAdmissionExported(journey) { exportMRNs =>
          checkExportMovementReferenceNumbersForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  journey,
                  BadRequest(
                    checkExportMovementReferenceNumbersPage(
                      exportMRNs,
                      formWithErrors,
                      routes.CheckExportMovementReferenceNumbersController.submit,
                      routes.EnterExportMovementReferenceNumberController.showNext,
                      routes.CheckExportMovementReferenceNumbersController.delete
                    )
                  )
                ),
              decision =>
                decision match {
                  case Yes =>
                    (
                      journey,
                      Redirect(
                        routes.EnterExportMovementReferenceNumberController.showNext(exportMRNs.size + 1)
                      )
                    )
                  case No  =>
                    if journey.userHasSeenCYAPage
                    then (journey, Redirect(routes.CheckYourAnswersController.show))
                    else (journey, Redirect(nextStepInJourney(journey)))
                }
            )

        },
    fastForwardToCYAEnabled = false
  )

  def delete(mrn: MRN): Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      journey =>
        whenTemporaryAdmissionExported(journey) { exportMRNs =>
          if exportMRNs.contains(mrn) then
            journey
              .removeExportMovementReferenceNumber(mrn)
              .fold(
                error => {
                  logger.warn(s"Error occurred trying to remove MRN $mrn - `$error`")
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible))
                },
                updatedJourney =>
                  (
                    updatedJourney,
                    updatedJourney.answers.exportMovementReferenceNumbers match {
                      case Some(exportMRNs) if exportMRNs.nonEmpty =>
                        Redirect(routes.CheckExportMovementReferenceNumbersController.show)
                      case _                                       =>
                        Redirect(routes.ChooseExportMethodController.show)
                    }
                  )
              )
          else (journey, Redirect(routes.CheckExportMovementReferenceNumbersController.show))
        },
    fastForwardToCYAEnabled = false
  )

  private def whenTemporaryAdmissionExported(
    journey: SecuritiesJourney
  )(
    body: Seq[MRN] => (SecuritiesJourney, Result)
  )(implicit request: Request[?]): (SecuritiesJourney, Result) =
    (journey.getReasonForSecurity, journey.getMethodOfDisposal, journey.answers.exportMovementReferenceNumbers) match {
      case (None, _, _)                                                                                 =>
        (journey, errorHandler.errorResult())
      case (Some(rfs), Some(mods), Some(exportMRNs)) if ntas.contains(rfs) && containsExportedMod(mods) =>
        body(exportMRNs)
      case (Some(rfs), Some(mod), None) if ntas.contains(rfs) && containsExportedMod(mod)               =>
        (journey, Redirect(routes.EnterExportMovementReferenceNumberController.showFirst))
      case (Some(rfs), Some(mod), _) if ntas.contains(rfs) && !containsExportedMod(mod)                 =>
        (journey, Redirect(nextStepInJourney(journey)))
      case (Some(rfs), None, _) if ntas.contains(rfs)                                                   =>
        (journey, Redirect(routes.ChooseExportMethodController.show))
      case (Some(_), _, _)                                                                              =>
        (journey, Redirect(nextStepInJourney(journey)))
    }

  private def containsExportedMod(mods: List[TemporaryAdmissionMethodOfDisposal]) =
    mods
      .filter(mod =>
        mod === TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments ||
          mod === TemporaryAdmissionMethodOfDisposal.ExportedInSingleOrMultipleShipments
      )
      .nonEmpty

}
