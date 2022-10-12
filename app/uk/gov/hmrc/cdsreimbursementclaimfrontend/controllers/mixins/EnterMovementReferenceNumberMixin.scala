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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CanSubmitMrnAndDeclaration

trait EnterMovementReferenceNumberMixin extends JourneyBaseController {

  type Journey <: uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.Journey with JourneyBase with CommonJourneyProperties with CanSubmitMrnAndDeclaration

  def modifyJourney(journey: Journey, mrn: MRN, declaration: DisplayDeclaration): Either[String, Journey]

  def claimService: ClaimService

  val form: Form[MRN]
  def viewTemplate: Form[MRN] => Request[_] => HtmlFormat.Appendable
  def afterSuccessfullSubmit(journey: Journey): Result

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        viewTemplate(
          form.withDefault(journey.getLeadMovementReferenceNumber)
        )(request)
      )
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Future.successful(
            (
              journey,
              BadRequest(
                viewTemplate(formWithErrors)(request)
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
            updatedJourney => (updatedJourney, afterSuccessfullSubmit(updatedJourney))
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
          modifyJourney(journey, mrn, acc14).left.map(Error.apply(_))
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }

  private def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]] =
    claimService
      .getDisplayDeclaration(mrn)

}
