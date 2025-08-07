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
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Form
import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.EnterExportMovementReferenceNumberController.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.containsExportedMethodsOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.containsMultipleExportedMethodsOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number_first
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number_next

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

@Singleton
class EnterExportMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterFirstExportMovementReferenceNumberPage: enter_export_movement_reference_number_first,
  enterNextExportMovementReferenceNumberPage: enter_export_movement_reference_number_next
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
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

  val showFirst: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form =
        journey.answers.exportMovementReferenceNumbers.flatMap(
          _.headOption
        ) match {
          case Some(exportMrn) =>
            // required to populate only MRN value
            firstExportMovementReferenceNumberForm
              .bind(Map(enterFirstExportMovementReferenceNumberKey -> exportMrn.value))
              .discardingErrors
          case None            =>
            firstExportMovementReferenceNumberForm
        }

      journey.getMethodOfDisposal match {
        case Some(mods) =>
          (if containsExportedMethodsOfDisposal(mods) then {
             (
               journey,
               Ok(
                 enterFirstExportMovementReferenceNumberPage(
                   form,
                   routes.EnterExportMovementReferenceNumberController.submitFirst
                 )
               )
             )
           } else {
             logger.error(
               "Should not reach this page as Method of disposal must be one of [ExportedInSingleShipment, ExportedInMultipleShipments, ExportedInSingleOrMultipleShipments]"
             )
             (journey, errorHandler.errorResult())

           }).asFuture
        case None       =>
          logger.error("Should not reach this page as method of disposal has not been selected yet.")
          (journey, errorHandler.errorResult()).asFuture
      }
    }
  }

  def showNext(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = nextExportMovementReferenceNumberForm
        .withDefault(
          journey.answers.exportMovementReferenceNumbers.flatMap(_.drop(pageIndex - 1).headOption)
        )
      journey.getMethodOfDisposal match {
        case Some(mods) =>
          (if containsMultipleExportedMethodsOfDisposal(mods) then {
             if journey.answers.exportMovementReferenceNumbers.map(_.size).exists(size => pageIndex <= size + 1) then
               (
                 journey,
                 Ok(
                   enterNextExportMovementReferenceNumberPage(
                     pageIndex,
                     form,
                     routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                   )
                 )
               )
             else
               // if pageIndex is outside the bounds
               (
                 journey,
                 Redirect(
                   journey.answers.exportMovementReferenceNumbers match {
                     case None       => routes.EnterExportMovementReferenceNumberController.showFirst
                     case Some(mrns) => routes.EnterExportMovementReferenceNumberController.showNext(mrns.size + 1)
                   }
                 )
               )
           } else {
             logger.error(
               "Should not reach this page as Method of disposal must be one of [ExportedInMultipleShipments, ExportedInSingleOrMultipleShipments]"
             )
             (journey, errorHandler.errorResult())
           }).asFuture
        case None       =>
          logger.error("Should not reach this page as method of disposal has not been selected yet.")
          (journey, errorHandler.errorResult()).asFuture
      }
    }
  }

  val submitFirst: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = firstExportMovementReferenceNumberForm
      form
        .bindFromRequest()
        .fold(
          (formWithErrors: Form[(MRN, YesNo)]) =>
            (
              journey,
              BadRequest(
                enterFirstExportMovementReferenceNumberPage(
                  formWithErrors,
                  routes.EnterExportMovementReferenceNumberController.submitFirst
                )
              )
            ).asFuture,
          { case (exportMrn, decision) =>
            claimService
              .getDisplayDeclaration(exportMrn)
              .fold(_ => None, identity)
              .map {
                case None              =>
                  // when import declaration does not exist with the given exportMRN
                  journey
                    .submitExportMovementReferenceNumber(0, exportMrn)
                    .fold(
                      {
                        case "submitExportMovementReferenceNumber.unexpected" =>
                          (journey, Redirect(nextStepInJourney(journey)))

                        case "submitExportMovementReferenceNumber.duplicated" =>
                          val updatedForm = form
                            .withError(
                              enterFirstExportMovementReferenceNumberKey,
                              "securities.error.duplicate-number"
                            )
                          (
                            journey,
                            BadRequest(
                              enterFirstExportMovementReferenceNumberPage(
                                updatedForm,
                                routes.EnterExportMovementReferenceNumberController.submitFirst
                              )
                            )
                          )
                        case _                                                =>
                          val updatedForm =
                            form.withError(enterFirstExportMovementReferenceNumberKey, "securities.error.import")
                          (
                            journey,
                            BadRequest(
                              enterFirstExportMovementReferenceNumberPage(
                                updatedForm,
                                routes.EnterExportMovementReferenceNumberController.submitFirst
                              )
                            )
                          )
                      },
                      updatedJourney =>
                        if updatedJourney.userHasSeenCYAPage then {
                          (updatedJourney, Redirect(routes.CheckYourAnswersController.show))
                        } else {
                          decision match {
                            case Yes =>
                              (
                                updatedJourney,
                                Redirect(routes.EnterExportMovementReferenceNumberController.showNext(2))
                              )

                            case No =>
                              // when there are already more export MRNs we must be in change mode and should display summary page
                              if journey.answers.exportMovementReferenceNumbers.exists(_.size > 1) then
                                (
                                  updatedJourney,
                                  Redirect(routes.CheckExportMovementReferenceNumbersController.show)
                                )
                              else
                                (
                                  updatedJourney,
                                  Redirect(nextStepInJourney(journey))
                                )
                          }
                        }
                    )
                // when import declaration exists with the given exportMRN
                case Some(declaration) =>
                  val formErrorKey = enterFirstExportMovementReferenceNumberKey
                  val updatedForm  =
                    form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                  (
                    journey,
                    BadRequest(
                      enterFirstExportMovementReferenceNumberPage(
                        updatedForm,
                        routes.EnterExportMovementReferenceNumberController.submitFirst
                      )
                    )
                  )
              }
          }
        )
    }
  }

  def submitNext(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = nextExportMovementReferenceNumberForm
      form
        .bindFromRequest()
        .fold(
          (formWithErrors: Form[MRN]) =>
            (
              journey,
              BadRequest(
                enterNextExportMovementReferenceNumberPage(
                  pageIndex,
                  formWithErrors,
                  routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                )
              )
            ).asFuture,
          exportMrn =>
            claimService
              .getDisplayDeclaration(exportMrn)
              .fold(_ => None, identity)
              .map {
                case None              =>
                  journey
                    .submitExportMovementReferenceNumber(pageIndex - 1, exportMrn)
                    .fold(
                      {
                        case "submitExportMovementReferenceNumber.unexpected" =>
                          (journey, Redirect(nextStepInJourney(journey)))

                        case "submitExportMovementReferenceNumber.duplicated" =>
                          val updatedForm = form
                            .withError(enterNextExportMovementReferenceNumberKey, "securities.error.duplicate-number")
                          (
                            journey,
                            BadRequest(
                              enterNextExportMovementReferenceNumberPage(
                                pageIndex,
                                updatedForm,
                                routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                              )
                            )
                          )
                        case _                                                =>
                          val updatedForm =
                            form.withError(enterNextExportMovementReferenceNumberKey, "securities.error.import")
                          (
                            journey,
                            BadRequest(
                              enterNextExportMovementReferenceNumberPage(
                                pageIndex,
                                updatedForm,
                                routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                              )
                            )
                          )
                      },
                      updatedJourney =>
                        if updatedJourney.userHasSeenCYAPage then {
                          (updatedJourney, Redirect(routes.CheckYourAnswersController.show))
                        } else {
                          (updatedJourney, Redirect(routes.CheckExportMovementReferenceNumbersController.show))
                        }
                    )
                // when import declaration exists with the given exportMRN
                case Some(declaration) =>
                  val formErrorKey = enterNextExportMovementReferenceNumberKey
                  val updatedForm  =
                    form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                  (
                    journey,
                    BadRequest(
                      enterNextExportMovementReferenceNumberPage(
                        pageIndex,
                        updatedForm,
                        routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                      )
                    )
                  )
              }
        )
    }
  }

  private def whenTemporaryAdmissionExported(
    journey: SecuritiesJourney
  )(body: => Future[(SecuritiesJourney, Result)])(implicit request: Request[?]): Future[(SecuritiesJourney, Result)] =
    (journey.getReasonForSecurity, journey.answers.temporaryAdmissionMethodsOfDisposal) match {
      case (None, _)                                                                                 =>
        (journey, errorHandler.errorResult()).asFuture
      case (Some(rfs), Some(mods)) if ntas.contains(rfs) && containsExportedMethodsOfDisposal(mods)  =>
        body
      case (Some(rfs), Some(mods)) if ntas.contains(rfs) && !containsExportedMethodsOfDisposal(mods) =>
        (journey, Redirect(nextStepInJourney(journey))).asFuture
      case (Some(rfs), None) if ntas.contains(rfs)                                                   =>
        (journey, Redirect(routes.ChooseExportMethodController.show)).asFuture
      case (Some(_), _)                                                                              =>
        (journey, Redirect(nextStepInJourney(journey))).asFuture
    }

}

object EnterExportMovementReferenceNumberController {
  val enterFirstExportMovementReferenceNumberKey: String = "enter-export-movement-reference-number"
  val enterNextExportMovementReferenceNumberKey: String  = "enter-export-movement-reference-number.next"

  val firstExportMovementReferenceNumberForm: Form[(MRN, YesNo)] =
    Form(
      mapping(
        enterFirstExportMovementReferenceNumberKey                       ->
          nonEmptyText
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value),
        s"$enterFirstExportMovementReferenceNumberKey.securities.yes-no" ->
          YesOrNoQuestionForm.yesNoMapping(
            s"$enterFirstExportMovementReferenceNumberKey.securities.yes-no"
          )
      )(Tuple2.apply)(Tuple2.unapply)
    )

  val nextExportMovementReferenceNumberForm: Form[MRN] =
    Form(
      mapping(
        enterNextExportMovementReferenceNumberKey ->
          nonEmptyText
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )
}
