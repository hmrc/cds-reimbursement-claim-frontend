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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.testonly.controllers

import javax.inject.{Inject, Singleton}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class FeatureSwitchController @Inject() (featureSwitch: FeatureSwitchService, mcc: MessagesControllerComponents)
    extends FrontendController(mcc) {

  def enable(featureName: String): Action[AnyContent] = Action {
    featureSwitch
      .of(featureName)
      .map(_.enable())
      .fold(NotFound(s"No $featureName feature exists"))(_ => Ok(s"Enabled feature $featureName"))
  }

  def disable(featureName: String): Action[AnyContent] = Action {
    featureSwitch
      .of(featureName)
      .map(_.disable())
      .fold(NotFound(s"No $featureName feature exists"))(_ => Ok(s"Disabled feature $featureName"))
  }
}
