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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class EnterMrnSchedulePlaceholderController @Inject() (
  featureSwitch: FeatureSwitchService,
  enterMrnSchedulePlaceholderPage: views.html.claims.enter_mrn_schedule_placeholder
)(implicit viewConfig: ViewConfig, val errorHandler: ErrorHandler, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with Logging {

  def show(): Action[AnyContent] = featureSwitch.BulkClaim.action.async { implicit request =>
    Future.successful(Ok(enterMrnSchedulePlaceholderPage()(request, cc.messagesApi.preferred(request), viewConfig)))
  }
}
