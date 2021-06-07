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

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterC285SingleDeclarationDetailsController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val enterDuplicateDeclarationDetailsPage: pages.enter_duplicate_declaration_details,
  val enterDeclarationDetailsPage: pages.enter_declaration_details
)(implicit
  val ec: ExecutionContext,
  val viewConfig: ViewConfig,
  val cc: MessagesControllerComponents,
  val errorHandler: ErrorHandler
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with DeclarationDetailsController[FillingOutClaim] {

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Future[Result], (SessionData, FillingOutClaim)]  =  //TODO: this FillingOutClaim type should be more specific
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: FillingOutClaim)) => Right(sessionData -> s)
      case _                                       => Left(Redirect(baseRoutes.StartController.start())) //TODO: this should go to the page where the journeys diverge
    }

  override protected val key: String                              = "enter-declaration-details.c285.single" //TODO: this enables dynamic content
  override protected val enterDeclarationDetailsCall: Call        =
    routes.EnterDeclarationDetailsController.enterDeclarationDetails()
  override protected val enterDeclarationDetailsSubmitCall: Call  =
    routes.EnterDeclarationDetailsController.enterDeclarationDetailsSubmit()
  override protected val changeDeclarationDetailsCall: Call       =
    routes.EnterDeclarationDetailsController.changeDeclarationDetails()
  override protected val changeDeclarationDetailsSubmitCall: Call =
    routes.EnterDeclarationDetailsController.changeDeclarationDetailsSubmit()

}
