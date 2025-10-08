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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

@Singleton
class ClaimControllerComponents @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val controllerComponents: MessagesControllerComponents,
  val sessionCache: SessionCache,
  val featureSwitchService: FeatureSwitchService,
  val errorHandler: ErrorHandler,
  val configuration: Configuration,
  val servicesConfig: ServicesConfig
) {

  val authenticatedActionWithSessionData =
    controllerComponents.messagesActionBuilder
      .compose(controllerComponents.actionBuilder)
      .andThen(authenticatedAction.readHeadersFromRequestOnly(false))
      .andThen(sessionDataAction.readHeadersFromRequestOnly(false))

  val authenticatedActionWithSessionDataWhenCallback =
    controllerComponents.messagesActionBuilder
      .compose(controllerComponents.actionBuilder)
      .andThen(authenticatedAction.readHeadersFromRequestOnly(true))
      .andThen(sessionDataAction.readHeadersFromRequestOnly(true))
}
