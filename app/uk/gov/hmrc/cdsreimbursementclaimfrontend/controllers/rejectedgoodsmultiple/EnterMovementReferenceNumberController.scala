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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes.subKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.EnterMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  def showFirst(): Action[AnyContent] = show(1)

  def show(pageIndex: Int): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (if (pageIndex <= 0 || pageIndex > journey.countOfMovementReferenceNumbers + 1)
       Redirect(routes.CheckMovementReferenceNumbersController.show())
     else {
       val mrnIndex: Int = pageIndex - 1

       Ok(
         enterMovementReferenceNumberPage(
           movementReferenceNumberForm.withDefault(journey.getNthMovementReferenceNumber(mrnIndex)),
           subKey,
           pageIndex,
           routes.EnterMovementReferenceNumberController.submit(pageIndex)
         )
       )
     }).asFuture
  }

  def submit(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    if (pageIndex <= 0 || pageIndex > journey.countOfMovementReferenceNumbers + 1)
      (
        journey,
        Redirect(routes.CheckMovementReferenceNumbersController.show())
      ).asFuture
    else {
      val mrnIndex: Int = pageIndex - 1
      movementReferenceNumberForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  subKey,
                  pageIndex,
                  routes.EnterMovementReferenceNumberController.submit(pageIndex)
                )
              )
            ).asFuture,
          mrn =>
            claimService
              .getDisplayDeclaration(mrn)
              .fold(
                errors => {
                  logger.error(s"Unable to record $mrn", errors.toException)
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                {
                  case Some(acc14) =>
                    journey
                      .submitMovementReferenceNumberAndDeclaration(mrnIndex, mrn, acc14)
                      .fold(
                        error =>
                          if (error === "submitMovementReferenceNumber.wrongDisplayDeclarationEori") {
                            (journey, BadRequest(customError(mrn, pageIndex, "multiple.error.wrongMRN")))
                          } else if (error === "submitMovementReferenceNumber.movementReferenceNumberAlreadyExists") {
                            (journey, BadRequest(customError(mrn, pageIndex, "multiple.error.existingMRN")))
                          } else {
                            logger.error(s"Unable to update journey [$error]")
                            (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                          },
                        updatedJourney => (updatedJourney, redirectLocation(journey, updatedJourney, mrn, pageIndex))
                      )
                  case None        =>
                    logger.error(s"Display Declaration details not found")
                    (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                }
              )
        )
    }
  }

  private def customError(mrn: MRN, pageIndex: Int, errorSuffix: String)(implicit request: Request[_]) =
    enterMovementReferenceNumberPage(
      movementReferenceNumberForm
        .fill(mrn)
        .withError(enterMovementReferenceNumberKey, errorSuffix),
      subKey,
      pageIndex,
      routes.EnterMovementReferenceNumberController.submit(pageIndex)
    )

  private def redirectLocation(
    journey: RejectedGoodsMultipleJourney,
    updatedJourney: RejectedGoodsMultipleJourney,
    mrn: MRN,
    pageIndex: Int
  ): Result =
    Redirect(
      if (updatedJourney.needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex)) {
        routes.EnterImporterEoriNumberController.show()
      } else {
        if (pageIndex === 1) routes.CheckDeclarationDetailsController.show()
        else if (journey.userHasSeenCYAPage && journey.getReimbursementClaimsFor(mrn).isEmpty)
          routes.SelectTaxCodesController.show(pageIndex)
        else routes.CheckMovementReferenceNumbersController.show()
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
