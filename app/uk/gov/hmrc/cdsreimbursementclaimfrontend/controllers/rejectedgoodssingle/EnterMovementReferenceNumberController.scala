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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.movementReferenceNumberForm
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends RejectedGoodsSingleJourneyBaseController(cc) {

  def show(): Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Future.successful(
      Ok(
        enterMovementReferenceNumberPage(
          movementReferenceNumberForm(),
          Some("rejected-goods.single"),
          routes.EnterMovementReferenceNumberController.show()
        )
      )
    )
  }
}
