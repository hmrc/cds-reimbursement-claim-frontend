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

import cats.data.{EitherT, NonEmptyList}
import cats.implicits.{catsSyntaxEq, catsSyntaxOptionId, toFoldableOps}
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAssociatedMrnController.mrnInputForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{AssociatedMRNsAnswer, AssociatedMrn}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{AssociatedMrnIndex, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterAssociatedMrnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  enterAssociatedMrnPage: pages.enter_associated_mrn,
  mrnDoesNotExistPage: pages.mrn_does_not_exist
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[AssociatedMRNsAnswer] = _.associatedMRNsAnswer

  def enterMrn(index: AssociatedMrnIndex): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(enterAssociatedMrnPage(index, mrnInputForm()))
  }

  def submitEnteredMrn(index: AssociatedMrnIndex): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (fillingOutClaim, associatedMRNs) =>
        def showError: Form[AssociatedMrn] => Future[Result] =
          formWithErrors => BadRequest(enterAssociatedMrnPage(index, formWithErrors))

        def addMrn(): AssociatedMrn => EitherT[Future, Error, Unit] = mrn =>
          EitherT {
            val references = associatedMRNs.map(_ :+ mrn) orElse AssociatedMRNsAnswer(mrn).some
            val claimCopy  = FillingOutClaim.of(fillingOutClaim)(_.copy(associatedMRNsAnswer = references))
            updateSession(sessionStore, request)(_.copy(journeyStatus = claimCopy.some))
          }

        def redirectToNextPage: EitherT[Future, Error, Unit] => Future[Result] = eitherFailOrSuccess =>
          eitherFailOrSuccess.fold(
            logAndDisplayError(s"Error storing ${index.ordinalNumeral} MRN: "),
            _ => Redirect(routes.CheckMovementReferenceNumbersController.showMrns())
          )

        mrnInputForm(fillingOutClaim.draftClaim.MRNs())
          .bindFromRequest()
          .fold(showError, addMrn().andThen(redirectToNextPage))
      }
    }

  def changeMrn(index: AssociatedMrnIndex): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (_, associatedMRNs) =>
        val idx = index.toRegular
        associatedMRNs.flatMap(_.get(idx.toLong)).fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
          Ok(enterAssociatedMrnPage(index, mrnInputForm().fill(mrn), editing = true))
        }
      }
  }

  def submitChangedMrn(index: AssociatedMrnIndex): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (fillingOutClaim, associatedMRNs) =>
        def showError: Form[AssociatedMrn] => Future[Result] =
          formWithErrors => BadRequest(enterAssociatedMrnPage(index, formWithErrors, editing = true))

        def updateAnswer(mrn: AssociatedMrn) = for {
          mrns    <- associatedMRNs.map(_.toList)
          idx     <- Option(index.toRegular).filter(mrns.indices.contains)
          (x, xs)  = mrns.splitAt(idx)
          updated <- NonEmptyList.fromList(x ++ (mrn :: xs.drop(1)))
        } yield updated

        def updateSessionAndThenRedirectToNextPage(): AssociatedMRNsAnswer => Future[Result] = answer => {
          val claim = FillingOutClaim.of(fillingOutClaim)(_.copy(associatedMRNsAnswer = answer.some))
          updateSession(sessionStore, request)(_.copy(journeyStatus = claim.some)).map(
            _.fold(
              logAndDisplayError(s"Error updating ${index.ordinalNumeral} MRN: "),
              _ => Redirect(routes.CheckMovementReferenceNumbersController.showMrns())
            )
          )
        }

        mrnInputForm(fillingOutClaim.draftClaim.MRNs())
          .bindFromRequest()
          .fold(
            showError,
            updateAnswer(_)
              .map(updateSessionAndThenRedirectToNextPage())
              .getOrElse(Future.successful(BadRequest(mrnDoesNotExistPage())))
          )
      }
    }
}

object EnterAssociatedMrnController {

  val enterAssociatedMrnKey: String = "enter-associated-mrn"

  def mrnInputForm(existing: List[MRN] = Nil): Form[AssociatedMrn] =
    Form(
      mapping(
        enterAssociatedMrnKey -> nonEmptyText
          .verifying("invalid.number", str => str.isEmpty || MRN.isValid(str))
          .verifying("error.exists", mrn => existing.forall(_.value =!= mrn))
          .transform[AssociatedMrn](MRN(_), _.value)
      )(identity)(Some(_))
    )

}
