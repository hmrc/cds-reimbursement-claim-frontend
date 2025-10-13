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
import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.firstExportMovementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.nextExportMovementReferenceNumberForm
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
  val jcc: ClaimControllerComponents,
  claimService: ClaimService,
  enterFirstExportMovementReferenceNumberPage: enter_export_movement_reference_number_first,
  enterNextExportMovementReferenceNumberPage: enter_export_movement_reference_number_next
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  private val enterFirstExportMovementReferenceNumberKey: String = "enter-export-movement-reference-number"
  private val enterNextExportMovementReferenceNumberKey: String  = "enter-export-movement-reference-number.next"

  def nextStepInClaim(claim: SecuritiesClaim)(using HeaderCarrier) =
    if claim.isSingleSecurity
    then routes.ChoosePayeeTypeController.show
    else routes.ConfirmFullRepaymentController.showFirst

  val showFirst: Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    whenTemporaryAdmissionExported(claim) {
      val form =
        claim.answers.exportMovementReferenceNumbers.flatMap(
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

      claim.getMethodOfDisposal match {
        case Some(mods) =>
          (if containsExportedMethodsOfDisposal(mods) then {
             (
               claim,
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
             (claim, errorHandler.errorResult())

           }).asFuture
        case None       =>
          logger.error("Should not reach this page as method of disposal has not been selected yet.")
          (claim, errorHandler.errorResult()).asFuture
      }
    }
  }

  def showNext(pageIndex: Int): Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    whenTemporaryAdmissionExported(claim) {
      val form = nextExportMovementReferenceNumberForm
        .withDefault(
          claim.answers.exportMovementReferenceNumbers.flatMap(_.drop(pageIndex - 1).headOption.map(x => (x, None)))
        )

      claim.getMethodOfDisposal match {
        case Some(mods) =>
          (if containsMultipleExportedMethodsOfDisposal(mods) then {
             if claim.answers.exportMovementReferenceNumbers.map(_.size).exists(size => pageIndex <= size + 1) then
               (
                 claim,
                 Ok(
                   enterNextExportMovementReferenceNumberPage(
                     pageIndex,
                     claim.userHasSeenCYAPage
                       && claim.answers.exportMovementReferenceNumbers.map(_.size).exists(size => pageIndex == size),
                     form,
                     routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                   )
                 )
               )
             else
               // if pageIndex is outside the bounds
               (
                 claim,
                 Redirect(
                   claim.answers.exportMovementReferenceNumbers match {
                     case None       => routes.EnterExportMovementReferenceNumberController.showFirst
                     case Some(mrns) => routes.EnterExportMovementReferenceNumberController.showNext(mrns.size + 1)
                   }
                 )
               )
           } else {
             logger.error(
               "Should not reach this page as Method of disposal must be one of [ExportedInMultipleShipments, ExportedInSingleOrMultipleShipments]"
             )
             (claim, errorHandler.errorResult())
           }).asFuture
        case None       =>
          logger.error("Should not reach this page as method of disposal has not been selected yet.")
          (claim, errorHandler.errorResult()).asFuture
      }
    }
  }

  val submitFirst: Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim =>
        whenTemporaryAdmissionExported(claim) {
          val form = firstExportMovementReferenceNumberForm
          form
            .bindFromRequest()
            .fold(
              (formWithErrors: Form[(MRN, YesNo)]) =>
                (
                  claim,
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
                      claim
                        .submitExportMovementReferenceNumber(0, exportMrn)
                        .fold(
                          {
                            case "submitExportMovementReferenceNumber.unexpected" =>
                              (claim, Redirect(nextStepInClaim(claim)))

                            case "submitExportMovementReferenceNumber.duplicated" =>
                              val updatedForm = form
                                .withError(
                                  enterFirstExportMovementReferenceNumberKey,
                                  "securities.error.duplicate-number"
                                )
                              (
                                claim,
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
                                claim,
                                BadRequest(
                                  enterFirstExportMovementReferenceNumberPage(
                                    updatedForm,
                                    routes.EnterExportMovementReferenceNumberController.submitFirst
                                  )
                                )
                              )
                          },
                          updatedClaim =>
                            decision match {
                              case Yes =>
                                (
                                  updatedClaim,
                                  Redirect(routes.EnterExportMovementReferenceNumberController.showNext(2))
                                )

                              case No =>
                                // when there are already more export MRNs we must be in change mode and should display summary page
                                if claim.answers.exportMovementReferenceNumbers.exists(_.size > 1) then
                                  (
                                    updatedClaim,
                                    Redirect(routes.CheckExportMovementReferenceNumbersController.show)
                                  )
                                else
                                  (
                                    updatedClaim,
                                    Redirect(nextStepInClaim(claim))
                                  )
                            }
                        )
                    // when import declaration exists with the given exportMRN
                    case Some(declaration) =>
                      val formErrorKey = enterFirstExportMovementReferenceNumberKey
                      val updatedForm  =
                        form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                      (
                        claim,
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
        },
    fastForwardToCYAEnabled = false
  )

  def submitNext(pageIndex: Int): Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim =>
        whenTemporaryAdmissionExported(claim) {
          val form = nextExportMovementReferenceNumberForm
          form
            .bindFromRequest()
            .fold(
              (formWithErrors: Form[(MRN, Option[YesNo])]) =>
                (
                  claim,
                  BadRequest(
                    enterNextExportMovementReferenceNumberPage(
                      pageIndex,
                      claim.userHasSeenCYAPage
                        && claim.answers.exportMovementReferenceNumbers.map(_.size).exists(size => pageIndex == size),
                      formWithErrors,
                      routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                    )
                  )
                ).asFuture,
              (exportMrn, decision) =>
                claimService
                  .getDisplayDeclaration(exportMrn)
                  .fold(_ => None, identity)
                  .map {
                    case None              =>
                      claim
                        .submitExportMovementReferenceNumber(pageIndex - 1, exportMrn)
                        .fold(
                          {
                            case "submitExportMovementReferenceNumber.unexpected" =>
                              (claim, Redirect(nextStepInClaim(claim)))

                            case "submitExportMovementReferenceNumber.duplicated" =>
                              val updatedForm = form
                                .withError(
                                  enterNextExportMovementReferenceNumberKey,
                                  "securities.error.duplicate-number"
                                )
                              (
                                claim,
                                BadRequest(
                                  enterNextExportMovementReferenceNumberPage(
                                    pageIndex,
                                    claim.answers.exportMovementReferenceNumbers
                                      .map(_.size)
                                      .exists(size => pageIndex == size),
                                    updatedForm,
                                    routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                                  )
                                )
                              )
                            case _                                                =>
                              val updatedForm =
                                form.withError(enterNextExportMovementReferenceNumberKey, "securities.error.import")
                              (
                                claim,
                                BadRequest(
                                  enterNextExportMovementReferenceNumberPage(
                                    pageIndex,
                                    claim.answers.exportMovementReferenceNumbers
                                      .map(_.size)
                                      .exists(size => pageIndex == size),
                                    updatedForm,
                                    routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                                  )
                                )
                              )
                          },
                          updatedClaim =>
                            decision match {
                              case Some(Yes) =>
                                (
                                  updatedClaim,
                                  Redirect(routes.EnterExportMovementReferenceNumberController.showNext(pageIndex + 1))
                                )

                              case _ =>
                                if claim.answers.exportMovementReferenceNumbers
                                    .flatMap(_.drop(pageIndex - 1).headOption)
                                    .contains(exportMrn) && claim.userHasSeenCYAPage
                                then (updatedClaim, Redirect(routes.CheckYourAnswersController.show))
                                else (updatedClaim, Redirect(routes.CheckExportMovementReferenceNumbersController.show))
                            }
                        )
                    // when import declaration exists with the given exportMRN
                    case Some(declaration) =>
                      val formErrorKey = enterNextExportMovementReferenceNumberKey
                      val updatedForm  =
                        form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                      (
                        claim,
                        BadRequest(
                          enterNextExportMovementReferenceNumberPage(
                            pageIndex,
                            claim.answers.exportMovementReferenceNumbers
                              .map(_.size)
                              .exists(size => pageIndex == size),
                            updatedForm,
                            routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                          )
                        )
                      )
                  }
            )
        },
    fastForwardToCYAEnabled = false
  )

  private def whenTemporaryAdmissionExported(
    claim: SecuritiesClaim
  )(body: => Future[(SecuritiesClaim, Result)])(implicit request: Request[?]): Future[(SecuritiesClaim, Result)] =
    (claim.getReasonForSecurity, claim.answers.temporaryAdmissionMethodsOfDisposal) match {
      case (None, _)                                                                                 =>
        (claim, errorHandler.errorResult()).asFuture
      case (Some(rfs), Some(mods)) if ntas.contains(rfs) && containsExportedMethodsOfDisposal(mods)  =>
        body
      case (Some(rfs), Some(mods)) if ntas.contains(rfs) && !containsExportedMethodsOfDisposal(mods) =>
        (claim, Redirect(nextStepInClaim(claim))).asFuture
      case (Some(rfs), None) if ntas.contains(rfs)                                                   =>
        (claim, Redirect(routes.ChooseExportMethodController.show)).asFuture
      case (Some(_), _)                                                                              =>
        (claim, Redirect(nextStepInClaim(claim))).asFuture
    }

}
