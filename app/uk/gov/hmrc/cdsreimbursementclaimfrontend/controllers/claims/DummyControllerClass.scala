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

import cats.syntax.all._
import com.google.inject.Inject
import play.api.i18n.Messages.implicitMessagesProviderToMessages
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ContactName, Eori, SessionData, SignedInUserDetails, UserType}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.SubmitPage.syntax._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.ClaimType.{Bulk, Single}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateContent.syntax._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.JourneyService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.util.Random

class DummyControllerClass @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val journeyService: JourneyService,
  val errorHandler: ErrorHandler,
  displayJourney: pages.display_journey
)(implicit viewConfig: ViewConfig, cc: MessagesControllerComponents, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def test(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val eitherErrorOrJourney = for {
        _       <- journeyService.persist(
                     SessionData(
                       FillingOutClaim(
                         GGCredId("6145079961943419"),
                         SignedInUserDetails(
                           Email("user@test.com").some,
                           Eori("GB000000000000001"),
                           Email("user@test.com"),
                           ContactName("USER")
                         ),
                         DraftC285Claim.newDraftC285Claim,
                         if (new Random().nextInt(2) > 0) Single else Bulk
                       ).some,
                       UserType.Individual.some
                     )
                   )
        journey <- journeyService.getJourney
      } yield journey

      eitherErrorOrJourney.fold(
        error => {
          logger.warn(error.message)
          errorHandler.errorResult()
        },
        journey => journey.showPage[DummyControllerClass]((key, submitUrl) => Ok(displayJourney(key, submitUrl)))
      )
    }

  def testSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      journeyService.getJourney
        .map(journey => journey.getNextUrl[DummyControllerClass])
        .fold(
          error => {
            logger.warn(error.message)
            errorHandler.errorResult()
          },
          nextUrl => Redirect(nextUrl)
        )
    }
}
