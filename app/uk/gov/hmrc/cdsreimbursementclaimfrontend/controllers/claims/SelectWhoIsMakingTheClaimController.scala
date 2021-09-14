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

import cats.data.EitherT
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectWhoIsMakingTheClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  selectWhoIsMakingTheClaimPage: pages.select_who_is_making_the_claim
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[DeclarantTypeAnswer] = _.declarantTypeAnswer

  def selectDeclarantType(implicit journey: JourneyBindable): Action[AnyContent] = show(isAmend = false)
  def changeDeclarantType(implicit journey: JourneyBindable): Action[AnyContent] = show(isAmend = true)

  def show(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantTypeAnswer] { (_, answers, router) =>
        val emptyForm  = SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
        val filledForm = answers.fold(emptyForm)(emptyForm.fill)
        Ok(selectWhoIsMakingTheClaimPage(filledForm, isAmend, router))
      }
    }

  def selectDeclarantTypeSubmit(implicit journey: JourneyBindable): Action[AnyContent] = submit(isAmend = false)
  def changeDeclarantTypeSubmit(implicit journey: JourneyBindable): Action[AnyContent] = submit(isAmend = true)

  def submit(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantTypeAnswer] { (fillingOutClaim, _, router) =>
        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectWhoIsMakingTheClaimPage(formWithErrors, isAmend, router)),
            formOk => {
              val updatedJourney = FillingOutClaim.of(fillingOutClaim)(_.copy(declarantTypeAnswer = Option(formOk)))

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("Could not save Declarant Type"))
                .fold(
                  logAndDisplayError("Submit Declarant Type error: "),
                  _ =>
                    Redirect(
                      router.nextPageForWhoIsMakingTheClaim(fillingOutClaim.draftClaim.movementReferenceNumber, isAmend)
                    )
                )
            }
          )
      }

    }

}

object SelectWhoIsMakingTheClaimController {

  val whoIsMakingTheClaimKey: String = "select-who-is-making-the-claim"

  val chooseDeclarantTypeForm: Form[DeclarantTypeAnswer] =
    Form(
      mapping(
        whoIsMakingTheClaimKey -> number
          .verifying("invalid", a => a >= 0 && a < DeclarantTypeAnswer.items.size)
          .transform[DeclarantTypeAnswer](
            index => DeclarantTypeAnswer.items(index),
            importer => DeclarantTypeAnswer.items.indexOf(importer)
          )
      )(identity)(Some(_))
    )
}
