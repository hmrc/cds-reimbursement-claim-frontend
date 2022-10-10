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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import cats.data.EitherT
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyModifications
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.enter_movement_reference_number
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait EnterMovementReferenceNumberMixin[Journey <: JourneyBase[Journey] with CommonJourneyModifications[Journey]] {
  self: JourneyBaseController[Journey] =>

  def claimService: ClaimService
  def enterMovementReferenceNumberPage: enter_movement_reference_number

  def subKey: Option[String]

  def showCall: Call
  def submitCall: Call

  def redirectLocation(journey: Journey): Result

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        enterMovementReferenceNumberPage(
          Forms.movementReferenceNumberForm.withDefault(journey.getLeadMovementReferenceNumber),
          subKey,
          submitCall
        )
      )
    }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    Forms.movementReferenceNumberForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Future.successful(
            (
              journey,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  subKey,
                  submitCall
                )
              )
            )
          ),
        mrn =>
          {
            for {
              maybeAcc14     <- getDeclaration(mrn)
              updatedJourney <- updateJourney(journey, mrn, maybeAcc14)
            } yield updatedJourney
          }.fold(
            errors => {
              logger.error(s"Unable to record $mrn", errors.toException)
              (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
            },
            updatedJourney => (updatedJourney, redirectLocation(updatedJourney))
          )
      )
  }

  private def updateJourney(
    journey: Journey,
    mrn: MRN,
    maybeAcc14: Option[DisplayDeclaration]
  ): EitherT[Future, Error, Journey] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.fromEither[Future](
          journey.submitMovementReferenceNumberAndDeclaration(mrn, acc14).left.map(Error.apply(_))
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }

  private def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]] =
    claimService
      .getDisplayDeclaration(mrn)

}
