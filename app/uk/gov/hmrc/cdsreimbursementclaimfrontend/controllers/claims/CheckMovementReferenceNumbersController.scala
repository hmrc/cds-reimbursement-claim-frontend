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
import cats.data.OptionT
import cats.implicits.{catsSyntaxEq, catsSyntaxOptionId}
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckMovementReferenceNumbersController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

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

  implicit val dataExtractor: DraftC285Claim => Option[AssociatedMRNsAnswer] =
    _.associatedMRNsAnswer

  def showMrns(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.using({ case journey: FillingOutClaim =>
      Ok(checkMovementReferenceNumbersPage(journey.draftClaim.MRNs(), whetherAddAnotherMrnAnswerForm))
    })
  }

  def deleteMrn(mrnIndex: AssociatedMrnIndex): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (fillingOutClaim, maybeAssociatedMRNs) =>
        def redirectToShowMrnsPage() =
          Redirect(routes.CheckMovementReferenceNumbersController.showMrns())

        val idx = mrnIndex.toRegular

        val maybeUpdatedClaim = for {
          mrns    <- maybeAssociatedMRNs.map(_.toList)
          updated <- AssociatedMRNsAnswer(mrns.take(idx) ++ mrns.drop(idx + 1))
          claim    = FillingOutClaim.of(fillingOutClaim)(_.copy(associatedMRNsAnswer = updated.some))
        } yield claim

        OptionT
          .fromOption[Future](maybeUpdatedClaim)
          .semiflatMap(claim => updateSession(sessionStore, request)(_.copy(journeyStatus = claim.some)))
          .fold(redirectToShowMrnsPage())(
            _.fold(
              logAndDisplayError(s"Error updating MRNs removing ${mrnIndex.ordinalNumeral} MRN: "),
              _ => redirectToShowMrnsPage()
            )
          )
      }
  }

  def submitMrns(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.using({ case journey: FillingOutClaim =>
      whetherAddAnotherMrnAnswerForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(checkMovementReferenceNumbersPage(journey.draftClaim.MRNs(), formWithErrors)),
          {
            case YesAddAnotherMrn   =>
              Redirect(routes.EnterAssociatedMrnController.enterMrn(journey.draftClaim.MRNs.total + 1))
            case DoNotAddAnotherMrn =>
              Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Multiple))
          }
        )
    })
  }
}

object CheckMovementReferenceNumbersController {

  val checkMovementReferenceNumbersKey = "check-movement-reference-numbers"

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
