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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.i18n.MessagesApi
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.EnterMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, messagesApi: MessagesApi)
    extends RejectedGoodsMultipleJourneyBaseController
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  def show(): Action[AnyContent] = showMrn(0) // For lead MRN

  def showMrn(leadOrOrdinalValue: Int): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      val emptyForm = movementReferenceNumberForm
      val form      =
        if (leadOrOrdinalValue === 0) journey.getLeadMovementReferenceNumber.fold(emptyForm)(emptyForm.fill)
        else emptyForm
      Ok(
        enterMovementReferenceNumberPage(
          form,
          ReimbursementRoutes(JourneyBindable.Multiple).subKey,
          getOrdinalValue(leadOrOrdinalValue),
          routes.EnterMovementReferenceNumberController.enterMrnSubmit(leadOrOrdinalValue)
        )
      )
    }
  }

  def enterMrnSubmit(leadOrOrdinalValue: Int): Action[AnyContent] = actionReadWriteJourney {
    implicit request => journey =>
      movementReferenceNumberForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  ReimbursementRoutes(JourneyBindable.Multiple).subKey,
                  getOrdinalValue(leadOrOrdinalValue),
                  routes.EnterMovementReferenceNumberController.enterMrnSubmit(leadOrOrdinalValue)
                )
              )
            ).asFuture,
          mrn => {
            val index = getOrdinalValue(leadOrOrdinalValue) - 1
            claimService
              .getDisplayDeclaration(mrn)
              .fold(
                errors => {
                  logger.error(s"Unable to record $mrn", errors.toException)
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                {
                  case Some(acc14) =>
                    if (index =!= 0 && journey.newMrnEorisAndLeadMrnEorisMismatch(acc14)) {
                      (journey, BadRequest(customError(mrn, leadOrOrdinalValue, "multiple.error.wrongMRN")))
                    } else if (index =!= 0 && journey.movementReferenceNumberExists(mrn)) {
                      (journey, BadRequest(customError(mrn, leadOrOrdinalValue, "multiple.error.existingMRN")))
                    } else {
                      journey
                        .submitMovementReferenceNumberAndDeclaration(index, mrn, acc14)
                        .fold(
                          error => {
                            logger.error(s"Unable to update journey [$error]")
                            (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                          },
                          updatedJourney => (updatedJourney, redirectLocation(updatedJourney, index))
                        )
                    }
                  case None        =>
                    logger.error(s"Display Declaration details not found")
                    (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                }
              )
          }
        )
  }

  private def customError(mrn: MRN, ordinalValue: Int, errorSuffix: String)(implicit request: Request[_]) =
    enterMovementReferenceNumberPage(
      movementReferenceNumberForm
        .fill(mrn)
        .withError(enterMovementReferenceNumberKey, errorSuffix),
      ReimbursementRoutes(JourneyBindable.Multiple).subKey,
      getOrdinalValue(ordinalValue),
      routes.EnterMovementReferenceNumberController.enterMrnSubmit(ordinalValue)
    )

  private def redirectLocation(journey: RejectedGoodsMultipleJourney, index: Int): Result =
    if (journey.needsDeclarantAndConsigneeEoriSubmission) {
      Redirect(routes.EnterImporterEoriNumberController.show())
    } else {
      Redirect(
        if (index === 0) routes.CheckDeclarationDetailsController.show()
        else routes.WorkInProgressController.show() //TODO: check-movement-reference-numbers (CDSR-1350)
      )
    }

  private def getOrdinalValue(value: Int): Int = if (value === 0) 1 else value

}

object EnterMovementReferenceNumberController {

  val enterMovementReferenceNumberKey: String = "enter-movement-reference-number.rejected-goods"

  val movementReferenceNumberForm: Form[MRN] =
    Form(
      mapping(
        enterMovementReferenceNumberKey ->
          nonEmptyText
            .verifying(
              "invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )
}
