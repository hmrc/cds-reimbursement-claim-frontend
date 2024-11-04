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
import com.github.arturopala.validator.Validator.Validate
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.EnterExportMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.ExportedInSingleOrMultipleShipments
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number_first
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_export_movement_reference_number_next

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterExportMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterExportMovementReferenceNumberFirstPage: enter_export_movement_reference_number_first,
  enterExportMovementReferenceNumberNextPage: enter_export_movement_reference_number_next
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val nextStepInJourney = routes.EnterContactDetailsController.show

  val showFirst: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = getForm(journey).withDefault(journey.answers.exportMovementReferenceNumbers.flatMap(_.headOption))
      journey.getMethodOfDisposal match {
        case Some(mod) =>
          (mod match {
            case ExportedInSingleShipment | ExportedInMultipleShipments | ExportedInSingleOrMultipleShipments =>
              (
                journey,
                Ok(
                  enterExportMovementReferenceNumberFirstPage(
                    form,
                    routes.EnterExportMovementReferenceNumberController.submitFirst
                  )
                )
              )
            case _                                                                                            =>
              logger.error(
                "Should not reach this page as Method of disposal must be one of [ExportedInSingleShipment, ExportedInMultipleShipments, ExportedInSingleOrMultipleShipments]"
              )
              (journey, errorHandler.errorResult())
          }).asFuture
        case None      =>
          logger.error("Should not reach this page as method of disposal has not been selected yet.")
          (journey, errorHandler.errorResult()).asFuture
      }
    }
  }

  def showNext(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    if (pageIndex === 1)
      Future.successful((journey, Redirect(routes.EnterExportMovementReferenceNumberController.showFirst)))
    else
      whenTemporaryAdmissionExported(journey) {
        val form = getForm(journey).withDefault(journey.answers.exportMovementReferenceNumbers.flatMap(_.headOption))
        journey.getMethodOfDisposal match {
          case Some(mod) =>
            (mod match {
              case ExportedInMultipleShipments | ExportedInSingleOrMultipleShipments =>
                (
                  journey,
                  Ok(
                    enterExportMovementReferenceNumberNextPage(
                      pageIndex,
                      form,
                      routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                    )
                  )
                )
              case _                                                                 =>
                logger.error(
                  "Should not reach this page as Method of disposal must be one of [ExportedInMultipleShipments, ExportedInSingleOrMultipleShipments]"
                )
                (journey, errorHandler.errorResult())
            }).asFuture
          case None      =>
            logger.error("Should not reach this page as method of disposal has not been selected yet.")
            (journey, errorHandler.errorResult()).asFuture
        }
      }
  }

  val submitFirst: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = getForm(journey)
      form
        .bindFromRequest()
        .fold(
          (formWithErrors: Form[MRN]) =>
            (
              journey,
              BadRequest(
                enterExportMovementReferenceNumberFirstPage(
                  formWithErrors,
                  routes.EnterExportMovementReferenceNumberController.submitFirst
                )
              )
            ).asFuture,
          mrn =>
            claimService
              .getDisplayDeclaration(mrn)
              .fold(
                _ => submitMrnAndContinue(0, mrn, journey),
                _ => {
                  val formErrorKey =
                    if (journey.getMethodOfDisposal.exists(_.value === ExportedInMultipleShipments))
                      enterExportMovementReferenceNumberMultipleKey
                    else enterExportMovementReferenceNumberSingleKey
                  val updatedForm  =
                    form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                  (
                    journey,
                    BadRequest(
                      enterExportMovementReferenceNumberFirstPage(
                        updatedForm,
                        routes.EnterExportMovementReferenceNumberController.submitFirst
                      )
                    )
                  )
                }
              )
        )
    }
  }

  def submitNext(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    whenTemporaryAdmissionExported(journey) {
      val form = getForm(journey)
      form
        .bindFromRequest()
        .fold(
          (formWithErrors: Form[MRN]) =>
            (
              journey,
              BadRequest(
                enterExportMovementReferenceNumberNextPage(
                  pageIndex,
                  formWithErrors,
                  routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                )
              )
            ).asFuture,
          mrn =>
            claimService
              .getDisplayDeclaration(mrn)
              .fold(
                _ => submitMrnAndContinue(pageIndex - 1, mrn, journey),
                _ => {
                  val formErrorKey =
                    if (journey.getMethodOfDisposal.exists(_.value === ExportedInMultipleShipments))
                      enterExportMovementReferenceNumberMultipleKey
                    else enterExportMovementReferenceNumberSingleKey
                  val updatedForm  =
                    form.copy(data = Map.empty, errors = List(FormError(formErrorKey, "securities.error.import")))
                  (
                    journey,
                    BadRequest(
                      enterExportMovementReferenceNumberNextPage(
                        pageIndex,
                        updatedForm,
                        routes.EnterExportMovementReferenceNumberController.submitNext(pageIndex)
                      )
                    )
                  )
                }
              )
        )
    }
  }

  private def submitMrnAndContinue(mrnIndex: Int, mrn: MRN, journey: SecuritiesJourney)(implicit
    request: Request[_]
  ): (SecuritiesJourney, Result) =
    journey
      .submitExportMovementReferenceNumber(mrnIndex, mrn)
      .fold(
        {
          case "submitExportMovementReferenceNumber.unexpected" =>
            (journey, Redirect(nextStepInJourney))
          case _                                                =>
            val formErrorKey =
              if (journey.getMethodOfDisposal.exists(_.value === ExportedInMultipleShipments))
                enterExportMovementReferenceNumberMultipleKey
              else enterExportMovementReferenceNumberSingleKey
            val updatedForm  = getForm(journey).withError(formErrorKey, "securities.error.import")
            (
              journey,
              BadRequest(
                if (mrnIndex == 0)
                  enterExportMovementReferenceNumberFirstPage(
                    updatedForm,
                    routes.EnterExportMovementReferenceNumberController.submitFirst
                  )
                else
                  enterExportMovementReferenceNumberNextPage(
                    mrnIndex + 1,
                    updatedForm,
                    routes.EnterExportMovementReferenceNumberController.submitNext(mrnIndex + 1)
                  )
              )
            )
        },
        updatedJourney =>
          if (updatedJourney.userHasSeenCYAPage) {
            (updatedJourney, Redirect(routes.CheckYourAnswersController.show))
          } else {
            (
              updatedJourney.withEnterContactDetailsMode(true),
              Redirect(nextStepInJourney)
            )
          }
      )

  private def whenTemporaryAdmissionExported(
    journey: SecuritiesJourney
  )(body: => Future[(SecuritiesJourney, Result)])(implicit request: Request[_]): Future[(SecuritiesJourney, Result)] =
    (journey.getReasonForSecurity, journey.answers.temporaryAdmissionMethodOfDisposal) match {
      case (None, _)                                                           =>
        (journey, errorHandler.errorResult()).asFuture
      case (Some(rfs), Some(mod)) if ntas.contains(rfs) && isExportedMod(mod)  =>
        body
      case (Some(rfs), Some(mod)) if ntas.contains(rfs) && !isExportedMod(mod) =>
        (journey, Redirect(nextStepInJourney)).asFuture
      case (Some(rfs), None) if ntas.contains(rfs)                             =>
        (journey, Redirect(routes.ChooseExportMethodController.show)).asFuture
      case (Some(_), _)                                                        =>
        (journey, Redirect(nextStepInJourney)).asFuture
    }

  private def isExportedMod(mod: TemporaryAdmissionMethodOfDisposal) =
    TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal.contains(mod)

  private def getForm(journey: SecuritiesJourney): Form[MRN] =
    journey.getMethodOfDisposal match {
      case Some(mod) =>
        mod match {
          case ExportedInSingleShipment            => exportMovementReferenceNumberSingleForm
          case ExportedInMultipleShipments         => exportMovementReferenceNumberMultipleForm
          case ExportedInSingleOrMultipleShipments => exportMovementReferenceNumberSingleForm
          case _                                   => exportMovementReferenceNumberSingleForm
        }
      case None      => exportMovementReferenceNumberSingleForm
    }

}

object EnterExportMovementReferenceNumberController {
  val enterExportMovementReferenceNumberSingleKey: String   = "enter-export-movement-reference-number"
  val enterExportMovementReferenceNumberMultipleKey: String = "enter-export-movement-reference-number.multiple"

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
