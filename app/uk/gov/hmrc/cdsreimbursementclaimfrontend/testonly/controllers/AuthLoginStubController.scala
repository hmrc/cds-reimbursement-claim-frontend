/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.testonly.controllers

import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.auth_login_stub_page
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class AuthLoginStubController @Inject() (
  val controllerComponents: MessagesControllerComponents,
  authLoginStubPage: auth_login_stub_page
)(implicit viewConfig: ViewConfig)
    extends FrontendBaseController {

  val defaultEoriNumber     = "GB000000000000001"
  val postAction: Call      = Call("POST", viewConfig.signInUrl)
  val startClaimUrl: String = s"${viewConfig.selfBaseUrl}${StartController.startNewClaim()}"

  final val show: Action[AnyContent] = Action { implicit request =>
    val eoriNumber = request.getQueryString("eori").getOrElse(defaultEoriNumber)
    Ok(authLoginStubPage(eoriNumber, startClaimUrl, postAction)).withNewSession
  }

}
