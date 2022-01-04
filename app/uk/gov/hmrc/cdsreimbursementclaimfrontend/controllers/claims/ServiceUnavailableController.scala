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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.service_unavailable
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class ServiceUnavailableController @Inject() (
  cc: MessagesControllerComponents,
  serviceUnavailablePage: service_unavailable
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc) {

  def unavailable(journey: JourneyBindable): Action[AnyContent] = Action { implicit request =>
    Ok(serviceUnavailablePage(journey))
  }
}
