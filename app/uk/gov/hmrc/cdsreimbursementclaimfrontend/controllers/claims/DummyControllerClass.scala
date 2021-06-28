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
import play.api.i18n.Messages.implicitMessagesProviderToMessages
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache2
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.{Journey, TemplateContent}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.Journey.{BulkJourney, SingleJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateContent._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

class DummyControllerClass @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache2,
  displayContent: pages.display_journey
)(implicit viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def testSingle(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(displayContent(resolveKey(SingleJourney("Single Journey!"))))
    }

  def testBulk(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(displayContent(resolveKey(BulkJourney("Bulk Journey!"))))
    }

  private def resolveKey[T <: Journey](journey: T)(implicit content: TemplateContent[T]) = {
    println(s"Hi from journey $journey")
    content.key
  }
}
