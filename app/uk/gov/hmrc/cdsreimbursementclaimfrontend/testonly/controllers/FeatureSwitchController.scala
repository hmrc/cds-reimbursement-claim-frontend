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

import play.api.mvc.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class FeatureSwitchController @Inject() (
  featureSwitch: FeatureSwitchService,
  val controllerComponents: MessagesControllerComponents
) extends FrontendBaseController {

  def enable(featureName: String): Action[AnyContent] = Action { implicit request =>
    Feature
      .of(featureName)
      .map(featureSwitch.enableForSession(_))
      .fold(NotFound(s"No $featureName feature exists"))(_ => Ok(s"Enabled feature $featureName"))
  }

  def disable(featureName: String): Action[AnyContent] = Action { implicit request =>
    Feature
      .of(featureName)
      .map(featureSwitch.disableForSession(_))
      .fold(NotFound(s"No $featureName feature exists"))(_ => Ok(s"Disabled feature $featureName"))
  }
}
