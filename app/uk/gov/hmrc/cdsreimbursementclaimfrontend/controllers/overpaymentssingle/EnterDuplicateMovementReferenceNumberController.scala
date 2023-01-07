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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import cats.data.EitherT
import cats.data.EitherT.fromEither
import cats.data.EitherT.fromOption
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterDuplicateMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  enterDuplicateMovementReferenceNumberPage: pages.enter_duplicate_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val controllerComponents: MessagesControllerComponents)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging
    with EnterMovementReferenceNumberMixin {

  implicit val journeyBindable: JourneyBindable = JourneyBindable.Single

  implicit val dataExtractor: DraftClaim => Option[MRN] =
    _.duplicateMovementReferenceNumberAnswer

  val duplicateMovementReferenceNumberKey: String = "enter-duplicate-movement-reference-number"

  val enterDuplicateMrn: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MRN] { (_, previousAnswer, router) =>
        val form = previousAnswer.fold(Forms.enterDuplicateMrnWithNoCheck)(Forms.enterDuplicateMrnWithNoCheck.fill)
        Ok(
          enterDuplicateMovementReferenceNumberPage(
            form,
            router.refNumberKey,
            routes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrnSubmit
          )
        )
      }
    }

  val enterDuplicateMrnSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MRN] { (fillingOutClaim, _, router) =>
        fillingOutClaim.draftClaim.movementReferenceNumber
          .fold(Forms.enterDuplicateMrnWithNoCheck)(Forms.enterDuplicateMrnCheckingAgainst)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterDuplicateMovementReferenceNumberPage(
                  requestFormWithErrors,
                  router.refNumberKey,
                  routes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrnSubmit
                )
              ),
            mrn => {
              val errorRedirect: Error => Result = e => {
                logger.warn("Mrn or Entry Number submission failed: ", e)
                Redirect(baseRoutes.IneligibleController.ineligible())
              }

              val result = for {
                maybeAcc14     <- claimService
                                    .getDisplayDeclaration(mrn)
                                    .leftMap(_ => Error("Could not get declaration"))
                mrnJourneyFlow <-
                  fromEither[Future](evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, maybeAcc14))
                    .leftMap(_ => Error("could not evaluate MRN flow"))
                declaration    <-
                  fromOption[Future](maybeAcc14, Error("could not unbox display declaration"))
                _              <-
                  EitherT(
                    updateSession(sessionStore, request)(
                      updateDuplicateMrnAndAcc14(fillingOutClaim, mrn, declaration)
                    )
                  )
                    .leftMap(_ => Error("Could not save Display Declaration"))
              } yield mrnJourneyFlow

              result.fold(errorRedirect, mrnJourney => Redirect(router.nextPageForDuplicateMRN(mrnJourney)))
            }
          )
      }
    }

  def updateDuplicateMRN(
    fillingOutClaim: FillingOutClaim,
    mrn: MRN
  ): SessionDataTransform = {
    val updatedDraftClaim =
      fillingOutClaim.draftClaim.copy(duplicateMovementReferenceNumberAnswer = Some(mrn))
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def updateDuplicateMrnAndAcc14(
    fillingOutClaim: FillingOutClaim,
    mrn: MRN,
    acc14: DisplayDeclaration
  ): SessionDataTransform = {
    val updatedDraftClaim = fillingOutClaim.draftClaim.copy(
      duplicateMovementReferenceNumberAnswer = Some(mrn),
      duplicateDisplayDeclaration = Option(acc14)
    )
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  override def updateDraftClaim(fillingOutClaim: FillingOutClaim, newDraftClaim: DraftClaim): SessionDataTransform = {
    val updatedJourney               = fillingOutClaim.copy(draftClaim = newDraftClaim)
    val update: SessionDataTransform = _.copy(journeyStatus = Some(updatedJourney))
    update
  }

}
