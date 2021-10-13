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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.Eq
import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.boolean
import play.api.data.Forms.mapping
import play.api.data.Forms.optional
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckMovementReferenceNumbersController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class CheckMovementReferenceNumbersController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  checkMovementReferenceNumbersPage: pages.check_movement_reference_numbers
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  def showMrns(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.using { case journey: FillingOutClaim =>
      Ok(checkMovementReferenceNumbersPage(journey.draftClaim.MRNs(), whetherAddAnotherMrnAnswerForm))
    }
  }

  def deleteMrn(mrnIndex: AssociatedMrnIndex): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      request.using { case journey: FillingOutClaim =>
        def redirectToShowMrnsPage() =
          Redirect(routes.CheckMovementReferenceNumbersController.showMrns())

        val updatedAssociatedMRNsAnswer            = journey.draftClaim.associatedMRNsAnswer.remove(mrnIndex)
        val updatedAssociatedMRNsDeclarationAnswer = journey.draftClaim.associatedMRNsDeclarationAnswer.remove(mrnIndex)
        val updatedClaim                           =
          FillingOutClaim.of(journey)(
            _.copy(
              associatedMRNsAnswer = updatedAssociatedMRNsAnswer,
              associatedMRNsDeclarationAnswer = updatedAssociatedMRNsDeclarationAnswer
            )
          )

        EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = updatedClaim.some)))
          .fold(
            logAndDisplayError(s"Error updating MRNs removing ${mrnIndex.ordinalNumeral} MRN: "),
            _ => redirectToShowMrnsPage()
          )
      }
  }

  def submitMrns(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.using { case journey: FillingOutClaim =>
      whetherAddAnotherMrnAnswerForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(checkMovementReferenceNumbersPage(journey.draftClaim.MRNs(), formWithErrors)),
          {
            case YesAddAnotherMrn   =>
              Redirect(
                routes.EnterAssociatedMrnController
                  .enterMrn(AssociatedMrnIndex.fromListIndex(journey.draftClaim.associatedMRNsAnswer.nextIndex))
              )
            case DoNotAddAnotherMrn =>
              Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Multiple))
          }
        )
    }
  }
}

object CheckMovementReferenceNumbersController {

  val checkMovementReferenceNumbersKey: String = "check-movement-reference-numbers"

  sealed trait WhetherAddAnotherMrnAnswer extends Product with Serializable
  case object YesAddAnotherMrn extends WhetherAddAnotherMrnAnswer
  case object DoNotAddAnotherMrn extends WhetherAddAnotherMrnAnswer

  implicit val addAnotherDocumentMrnEq: Eq[WhetherAddAnotherMrnAnswer] =
    Eq.fromUniversalEquals[WhetherAddAnotherMrnAnswer]

  val whetherAddAnotherMrnAnswerForm: Form[WhetherAddAnotherMrnAnswer] =
    Form(
      mapping(
        checkMovementReferenceNumbersKey -> optional(boolean)
          .verifying("invalid-answer", _.isDefined)
          .transform[WhetherAddAnotherMrnAnswer](
            opt => if (opt.exists(_ === true)) YesAddAnotherMrn else DoNotAddAnotherMrn,
            answer => Some(answer === YesAddAnotherMrn)
          )
      )(identity)(Some(_))
    )
}
