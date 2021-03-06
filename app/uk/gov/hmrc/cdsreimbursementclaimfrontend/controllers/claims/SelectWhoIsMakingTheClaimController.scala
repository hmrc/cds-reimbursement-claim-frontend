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
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectWhoIsMakingTheClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  selectWhoIsMakingTheClaimPage: pages.select_who_is_making_the_claim
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[DeclarantTypeAnswer] = _.declarantTypeAnswer

  def selectDeclarantType(journey: JourneyBindable): Action[AnyContent] = show(false)(journey)
  def changeDeclarantType(journey: JourneyBindable): Action[AnyContent] = show(true)(journey)

  def show(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantTypeAnswer] { (_, answers, router) =>
        val emptyForm  = SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
        val filledForm = answers
          .fold(emptyForm)(emptyForm.fill(_))
        Ok(selectWhoIsMakingTheClaimPage(filledForm, isAmend, router))
      }
    }

  def selectDeclarantTypeSubmit(journey: JourneyBindable): Action[AnyContent] = submit(false)(journey)
  def changeDeclarantTypeSubmit(journey: JourneyBindable): Action[AnyContent] = submit(true)(journey)

  def submit(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DeclarantTypeAnswer] { (fillingOutClaim, _, router) =>
        SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectWhoIsMakingTheClaimPage(formWithErrors, isAmend, router)),
            formOk => {
              val newDraftClaim  =
                fillingOutClaim.draftClaim
                  .fold(_.copy(declarantTypeAnswer = Option(formOk)))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("Could not save Declarant Type"))
                .fold(
                  e => {
                    logger.warn("Submit Declarant Type error: ", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(router.nextPageForWhoIsMakingTheClaim(isAmend))
                )
            }
          )
      }

    }

}

object SelectWhoIsMakingTheClaimController {

  val whoIsMakingTheClaimKey = "select-who-is-making-the-claim"

  val chooseDeclarantTypeForm: Form[DeclarantTypeAnswer] =
    Form(
      mapping(
        whoIsMakingTheClaimKey -> number
          .verifying("invalid", a => a === 0 || a === 1 || a === 2)
          .transform[DeclarantTypeAnswer](
            value =>
              if (value === 0) DeclarantTypeAnswer.Importer
              else if (value === 1) DeclarantTypeAnswer.AssociatedWithImporterCompany
              else DeclarantTypeAnswer.AssociatedWithRepresentativeCompany,
            {
              case DeclarantTypeAnswer.Importer                            => 0
              case DeclarantTypeAnswer.AssociatedWithImporterCompany       => 1
              case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany => 2
            }
          )
      )(identity)(Some(_))
    )
}
