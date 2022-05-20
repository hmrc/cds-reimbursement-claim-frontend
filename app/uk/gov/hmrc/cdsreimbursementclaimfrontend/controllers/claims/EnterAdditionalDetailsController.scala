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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAdditionalDetailsController.additionalDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AdditionalDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class EnterAdditionalDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterAdditionalDetailsPage: pages.enter_additional_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with Logging
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[AdditionalDetailsAnswer] = _.additionalDetailsAnswer

  def enterAdditionalDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[AdditionalDetailsAnswer] { (_, answers) =>
        val form = answers.toList.foldLeft(additionalDetailsForm)((form, answer) => form.fill(answer))
        Ok(enterAdditionalDetailsPage(form))
      }
    }

  def enterAdditionalDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[AdditionalDetailsAnswer] { (fillingOutClaim, _, router) =>
        import router._

        additionalDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors => BadRequest(enterAdditionalDetailsPage(requestFormWithErrors)),
            additionalDetails => {
              val newDraftClaim  = fillingOutClaim.draftClaim.copy(additionalDetailsAnswer = Some(additionalDetails))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("could not update session"))
                .fold(
                  logAndDisplayError("could not get additional details"),
                  _ =>
                    Redirect(
                      CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively = journeyBindable match {
                        case JourneyBindable.Scheduled =>
                          claimRoutes.SelectDutyTypesController.showDutyTypes(JourneyBindable.Scheduled)
                        case JourneyBindable.Multiple  =>
                          overpaymentsMultipleRoutes.SelectMultipleDutiesController.selectDuties(index = 1)
                        case _                         =>
                          claimRoutes.SelectDutiesController.selectDuties()
                      })
                    )
                )
            }
          )
      }
    }
}

object EnterAdditionalDetailsController {

  val additionalDetailsForm: Form[AdditionalDetailsAnswer] = Form(
    mapping("enter-additional-details" -> nonEmptyText(maxLength = 500))(AdditionalDetailsAnswer.apply)(
      AdditionalDetailsAnswer.unapply
    )
  )
}