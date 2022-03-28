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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      enterMovementReferenceNumberPage(
        movementReferenceNumberForm.withDefault(journey.getLeadMovementReferenceNumber),
        MRNScheduledRoutes.subKey,
        1,
        routes.EnterMovementReferenceNumberController.submit()
      )
    ).asFuture
  }

  def submit(): Action[AnyContent]                                                                            = actionReadWriteJourney { implicit request => journey =>
    movementReferenceNumberForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              enterMovementReferenceNumberPage(
                formWithErrors,
                MRNScheduledRoutes.subKey,
                1,
                routes.EnterMovementReferenceNumberController.submit()
              )
            )
          ).asFuture,
        mrn =>
          claimService
            .getDisplayDeclaration(mrn)
            .fold(
              errors => {
                logger.error(s"Unable to retrieve $mrn", errors.toException)
                (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
              },
              {
                case Some(acc14) =>
                  journey
                    .submitMovementReferenceNumberAndDeclaration(mrn, acc14)
                    .fold(
                      error =>
                        if (error === "submitMovementReferenceNumber.wrongDisplayDeclarationMrn") {
                          (journey, BadRequest(customError(mrn, "scheduled.error.wrongMRN")))
                        } else {
                          logger.error(s"Unable to update journey [$error]")
                          (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                        },
                      updatedJourney => (updatedJourney, redirectLocation(updatedJourney))
                    )
                case None        =>
                  logger.error(s"Display Declaration details not found")
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
              }
            )
      )
  }
  private def customError(mrn: MRN, errorSuffix: String)(implicit request: Request[_]): HtmlFormat.Appendable =
    enterMovementReferenceNumberPage(
      movementReferenceNumberForm
        .fill(mrn)
        .withError(enterMovementReferenceNumberKey, errorSuffix),
      MRNScheduledRoutes.subKey,
      1,
      routes.EnterMovementReferenceNumberController.submit()
    )
  private def redirectLocation(updatedJourney: RejectedGoodsScheduledJourney): Result                         =
    Redirect(
      if (updatedJourney.needsDeclarantAndConsigneeEoriSubmission) {
        routes.EnterImporterEoriNumberController.show()
      } else {
        routes.WorkInProgressController.show() //TODO: Should be routes.CheckDeclarationDetailsController.show()
      }
    )
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
