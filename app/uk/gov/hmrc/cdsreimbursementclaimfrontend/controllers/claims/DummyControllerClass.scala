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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ContactName, Eori, SessionData, SignedInUserDetails, UserType}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.SubmitPage._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateContent._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.Journey.{BulkJourney, SingleJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.{JourneyService, SubmitPage, TemplateContent}
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
    with SessionUpdates { self =>

  def test(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val draft   = DraftC285Claim.newDraftC285Claim
      val n       = new Random().nextInt(2)
      val journey = if (n > 0) SingleJourney(draft) else BulkJourney(draft)

      journeyService
        .persist(
          SessionData(
            FillingOutJourney(
              GGCredId("6145079961943419"),
              SignedInUserDetails(
                Email("user@test.com").some,
                Eori("GB000000000000001"),
                Email("user@test.com"),
                ContactName("USER")
              ),
              journey
            ).some,
            UserType.Individual.some
          )
        )
        .fold(
          error => {
            logger.warn(error.message)
            errorHandler.errorResult()
          },
          _ => {
            val template = journey match {
              case _: SingleJourney => implicitly[TemplateContent[DummyControllerClass, SingleJourney]]
              case _: BulkJourney   => implicitly[TemplateContent[DummyControllerClass, BulkJourney]]
            }

            Ok(displayJourney(template.key, template.submitUrlFor))
          }
        )
    }

  def testSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      journeyService.getJourney
        .map {
          case _: SingleJourney => implicitly[SubmitPage[DummyControllerClass, SingleJourney]].nextUrl
          case _: BulkJourney   => implicitly[SubmitPage[DummyControllerClass, BulkJourney]].nextUrl
        }
        .fold(
          error => {
            logger.warn(error.message)
            errorHandler.errorResult()
          },
          nextUrl => Redirect(nextUrl)
        )
    }
}
