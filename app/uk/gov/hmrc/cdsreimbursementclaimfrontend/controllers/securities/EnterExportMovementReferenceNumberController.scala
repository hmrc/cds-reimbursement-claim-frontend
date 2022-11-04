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

import cats.implicits.catsSyntaxEq
import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.FormError
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.EnterExportMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.temporaryAdmissions
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number_multiple

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterExportMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterExportMovementReferenceNumberSinglePage: enter_export_movement_reference_number,
  enterExportMovementReferenceNumberMultiplePage: enter_export_movement_reference_number_multiple
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = getForm(journey).withDefault(journey.answers.exportMovementReferenceNumber)
      journey.getMethodOfDisposal match {
        case Some(mod) =>
          (mod match {
            case ExportedInSingleShipment | ExportedInMultipleShipments =>
              (journey, getResultPage(journey, form, Ok))
            case _                                                      =>
              logger.error(
                "Should not reach this page as Method of disposal must be one of [ExportedInSingleShipment, ExportedInMultipleShipments]"
              )
              (journey, errorHandler.errorResult())
          }).asFuture
        case None      =>
          logger.error("Should not reach this page as method of disposal has not been selected yet.")
          (journey, errorHandler.errorResult()).asFuture
      }
    }
  }

  def submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = getForm(journey)
      form.bindFromRequest
        .fold(
          (formWithErrors: Form[MRN]) => (journey, getResultPage(journey, formWithErrors, BadRequest)).asFuture,
          mrn =>
            claimService
              .getDisplayDeclaration(mrn)
              .fold(
                _ => submitMrnAndContinue(mrn, journey),
                _ => {
                  val formErrorKey =
                    if (journey.getMethodOfDisposal.exists(_.value === ExportedInMultipleShipments))
                      enterExportMovementReferenceNumberMultipleKey
                    else enterExportMovementReferenceNumberSingleKey
                  val updatedForm  =
                    form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                  (journey, getResultPage(journey, updatedForm, BadRequest))
                }
              )
        )
    }
  }

  private def submitMrnAndContinue(mrn: MRN, journey: SecuritiesJourney)(implicit
    request: Request[_]
  ): (SecuritiesJourney, Result) =
    journey
      .submitExportMovementReferenceNumber(mrn)
      .fold(
        {
          case "submitExportMovementReferenceNumber.unexpected" =>
            (journey, Redirect(routes.CheckClaimantDetailsController.show()))
          case _                                                =>
            val formErrorKey =
              if (journey.getMethodOfDisposal.exists(_.value === ExportedInMultipleShipments))
                enterExportMovementReferenceNumberMultipleKey
              else enterExportMovementReferenceNumberSingleKey
            (
              journey,
              getResultPage(journey, getForm(journey).withError(formErrorKey, "securities.error.import"), BadRequest)
            )
        },
        updatedJourney =>
          if (updatedJourney.userHasSeenCYAPage) {
            (updatedJourney, Redirect(routes.CheckYourAnswersController.show()))
          } else {
            (updatedJourney, Redirect(routes.CheckClaimantDetailsController.show()))
          }
      )

  private def whenTemporaryAdmissionExported(
    journey: SecuritiesJourney
  )(body: => Future[(SecuritiesJourney, Result)])(implicit request: Request[_]): Future[(SecuritiesJourney, Result)] =
    (journey.getReasonForSecurity, journey.answers.temporaryAdmissionMethodOfDisposal) match {
      case (None, _)                                                                          =>
        (journey, errorHandler.errorResult()).asFuture
      case (Some(rfs), Some(mod)) if temporaryAdmissions.contains(rfs) && isExportedMod(mod)  =>
        body
      case (Some(rfs), Some(mod)) if temporaryAdmissions.contains(rfs) && !isExportedMod(mod) =>
        (journey, Redirect(routes.CheckClaimantDetailsController.show())).asFuture
      case (Some(rfs), None) if temporaryAdmissions.contains(rfs)                             =>
        (journey, Redirect(routes.ChooseExportMethodController.show())).asFuture
      case (Some(rfs), _) if !temporaryAdmissions.contains(rfs)                               =>
        (journey, Redirect(routes.CheckClaimantDetailsController.show())).asFuture
    }

  private def isExportedMod(mod: TemporaryAdmissionMethodOfDisposal) =
    TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal.contains(mod)

  private def getForm(journey: SecuritiesJourney): Form[MRN] =
    journey.getMethodOfDisposal match {
      case Some(mod) =>
        mod match {
          case ExportedInSingleShipment    => exportMovementReferenceNumberSingleForm
          case ExportedInMultipleShipments => exportMovementReferenceNumberMultipleForm
          case _                           => exportMovementReferenceNumberSingleForm
        }
      case None      => exportMovementReferenceNumberSingleForm
    }

  private def getResultPage(journey: SecuritiesJourney, form: Form[MRN], method: Status)(implicit
    request: Request[_],
    messages: Messages
  ): Result =
    journey.getMethodOfDisposal match {
      case Some(mod) =>
        mod match {
          case ExportedInSingleShipment    =>
            method(
              enterExportMovementReferenceNumberSinglePage(
                form,
                routes.EnterExportMovementReferenceNumberController.submit()
              )
            )
          case ExportedInMultipleShipments =>
            method(
              enterExportMovementReferenceNumberMultiplePage(
                form,
                routes.EnterExportMovementReferenceNumberController.submit()
              )
            )
          case _                           =>
            logger.error("Method of Disposal must be one of [ExportedInSingleShipment, ExportedInMultipleShipments]")
            errorHandler.errorResult()
        }
      case None      =>
        logger.error(
          "MethodOfDisposal is empty. Must be one of [ExportedInSingleShipment, ExportedInMultipleShipments]"
        )
        errorHandler.errorResult()
    }

}

object EnterExportMovementReferenceNumberController {
  val enterExportMovementReferenceNumberSingleKey: String   = "enter-export-movement-reference-number"
  val enterExportMovementReferenceNumberMultipleKey: String = "enter-export-movement-reference-number-multiple"

  val exportMovementReferenceNumberSingleForm: Form[MRN] =
    Form(
      mapping(
        enterExportMovementReferenceNumberSingleKey ->
          nonEmptyText
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  val exportMovementReferenceNumberMultipleForm: Form[MRN] =
    Form(
      mapping(
        enterExportMovementReferenceNumberMultipleKey ->
          nonEmptyText
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )
}
