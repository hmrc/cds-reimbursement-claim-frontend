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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class FeatureSwitchProtectedAction(feature: Feature, featureSwitch: FeatureSwitchService, errorHandler: ErrorHandler)(
  implicit val executionContext: ExecutionContext
) extends ActionRefiner[MessagesRequest, MessagesRequest]
    with Logging { self =>

  override protected def refine[A](request: MessagesRequest[A]): Future[Either[Result, MessagesRequest[A]]] =
    Future.successful(
      if (featureSwitch.isEnabled(feature)) Right(request)
      else Left(Results.NotFound(errorHandler.notFoundTemplate(request)))
    )

}
