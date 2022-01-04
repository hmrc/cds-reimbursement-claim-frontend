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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionDataAndRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.FeatureSwitchProtectedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import scala.concurrent.ExecutionContext
import play.api.mvc.MessagesRequest

@Singleton
class JourneyControllerComponents @Inject() (
  authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val controllerComponents: MessagesControllerComponents,
  val sessionCache: SessionCache,
  featureSwitchService: FeatureSwitchService,
  val errorHandler: ErrorHandler
)(implicit ec: ExecutionContext) {

  final val actionBuilder: ActionBuilder[MessagesRequest, AnyContent] =
    controllerComponents.messagesActionBuilder
      .compose(controllerComponents.actionBuilder)

  final def authenticatedActionWithSessionData(
    featureOpt: Option[Feature]
  ): ActionBuilder[RequestWithSessionData, AnyContent] =
    featureOpt match {
      case Some(feature) =>
        actionBuilder
          .andThen(new FeatureSwitchProtectedAction(feature, featureSwitchService, errorHandler))
          .andThen(authenticatedAction)
          .andThen(sessionDataAction)

      case None =>
        actionBuilder
          .andThen(authenticatedAction)
          .andThen(sessionDataAction)
    }

  final def authenticatedActionWithRetrievedDataAndSessionData(
    featureOpt: Option[Feature]
  ): ActionBuilder[RequestWithSessionDataAndRetrievedData, AnyContent] =
    featureOpt match {
      case Some(feature) =>
        actionBuilder
          .andThen(new FeatureSwitchProtectedAction(feature, featureSwitchService, errorHandler))
          .andThen(authenticatedActionWithRetrievedData)
          .andThen(sessionDataActionWithRetrievedData)

      case None =>
        actionBuilder
          .andThen(authenticatedActionWithRetrievedData)
          .andThen(sessionDataActionWithRetrievedData)

    }

}
