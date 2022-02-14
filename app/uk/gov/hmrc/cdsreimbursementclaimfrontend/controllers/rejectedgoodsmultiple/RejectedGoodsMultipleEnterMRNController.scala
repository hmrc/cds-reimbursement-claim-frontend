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

import cats.data.EitherT
import cats.syntax.all._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class RejectedGoodsMultipleEnterMRNController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends RejectedGoodsMultipleJourneyBaseController
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  import cats.data.EitherT._
  implicit val dataExtractor: DraftClaim => Option[MRN] = _.movementReferenceNumber

  def enterJourneyMrn(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      val emptyForm = movementReferenceNumberForm
      val form      = journey.getLeadMovementReferenceNumber.fold(emptyForm)(emptyForm.fill)
      val router    = ReimbursementRoutes(JourneyBindable.Multiple)
      Ok(
        enterMovementReferenceNumberPage(
          form,
          router.subKey,
          routes.RejectedGoodsMultipleEnterMRNController.enterMrnSubmit()
        )
      )
    }
  }

  def enterMrnSubmit(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    movementReferenceNumberForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          BadRequest(
            enterMovementReferenceNumberPage(
              formWithErrors,
              ReimbursementRoutes(JourneyBindable.Multiple).subKey,
              routes.RejectedGoodsMultipleEnterMRNController.enterMrnSubmit()
            )
          ),
        mrnNumber => {

          val isSameAsPrevious =
            journey.answers.movementReferenceNumbers.map(mrnSeq => mrnSeq).exists(_.contains(mrnNumber))

          if (isSameAsPrevious && journey.hasCompleteAnswers)
            Future.successful(Redirect(routes.CheckYourAnswersController.show()))
          else {
            for {
              maybeAcc14    <-
                claimService.getDisplayDeclaration(mrnNumber).leftMap(_ => Error("Could not get declaration"))
              updateJourney <- updateJourney(journey, mrnNumber, maybeAcc14)
            } yield updateJourney
          }.fold(
            errors => {
              logger.error(s"Unable to record $mrnNumber", errors.toException)
              Redirect(baseRoutes.IneligibleController.ineligible())
            },
            updatedJourney => redirectLocation(updatedJourney)
          )
        }
      )
  }

  private def updateJourney(
    journey: RejectedGoodsMultipleJourney,
    mrn: MRN,
    maybeAcc14: Option[DisplayDeclaration]
  ): EitherT[Future, Error, RejectedGoodsMultipleJourney] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.fromEither[Future](
          journey.submitMovementReferenceNumberAndDeclaration(mrn, acc14).left.map(Error.apply(_))
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }

  private def redirectLocation(journey: RejectedGoodsMultipleJourney): Result =
    if (journey.needsDeclarantAndConsigneeEoriSubmission) {
      Redirect(Call("GET", "enter-importer-eori-number"))
    } else {
      Redirect(Call("GET", "get-declaration-details"))
    }

}
