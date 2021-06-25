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

import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache2
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.syntax._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

class DummyControllerClass @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache2,
  cc: MessagesControllerComponents,
  enterDeclarationDetailsPage: views.html.enter_declaration_details
  // inject template???
  //
  // Template1
  // Template2
)(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def test(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    // internal data structure in memory <- uuid
    sessionCache
      .get()
      .fold(
        error => InternalServerError(s"something bad has happened: ${error.message}"),
        {

          // Twirl template associated to it
          // Depending on what journey type, we need to show different content in that template
          val someInfo = getJourneyMeta(journey)
          // submit Call, message Key
          case Some(value) => Ok(enterDeclarationDetailsPage(someInfo)) //value.journeyStatus.draftClaim))
          case None        => Ok(enterDeclarationDetailsPage(emptyForm))
        }
      )
  }

}
