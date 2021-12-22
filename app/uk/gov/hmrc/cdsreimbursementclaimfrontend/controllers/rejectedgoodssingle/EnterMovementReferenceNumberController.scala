/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.data.EitherT
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends RejectedGoodsSingleJourneyBaseController {

  private val subKey = Some("rejected-goods.single")

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        enterMovementReferenceNumberPage(
          movementReferenceNumberForm.withDefault(journey.answers.movementReferenceNumber),
          subKey,
          routes.EnterMovementReferenceNumberController.submit()
        )
      )
    }
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    movementReferenceNumberForm
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
                  routes.EnterMovementReferenceNumberController.submit()
                )
              )
            )
          ),
        mrn =>
          {
            for {
              maybeAcc14    <- getDeclaration(mrn)
              updateJourney <- updateJourney(journey, mrn, maybeAcc14)
            } yield updateJourney
          }.fold(
            errors => {
              logger.error(s"Unable to record $mrn", errors.toException)
              (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
            },
            updatedJourney => (updatedJourney, redirectLocation(updatedJourney))
          )
      )
  }

  private def redirectLocation(journey: RejectedGoodsSingleJourney): Result =
    if (journey.needsDeclarantAndConsigneeEoriSubmission) {
      Redirect(routes.EnterImporterEoriNumberController.show())
    } else {
      Redirect(routes.CheckDeclarationDetailsController.show())
    }

  private def updateJourney(
    journey: RejectedGoodsSingleJourney,
    mrn: MRN,
    maybeAcc14: Option[DisplayDeclaration]
  ): EitherT[Future, Error, RejectedGoodsSingleJourney] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.rightT(
          journey
            .submitMovementReferenceNumber(mrn)
            .submitDisplayDeclaration(acc14)
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }

  private def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]] =
    claimService
      .getDisplayDeclaration(mrn)
}
