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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass

import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.Journey.{BulkJourney, SingleJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{DummyControllerClass, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.Journey
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

trait TemplateContent[C <: FrontendController, T <: Journey] {
  val key: String

  def submitUrlFor(journey: Journey): Call =
    routes.DummyControllerClass.testSubmit(journey.id)
}

object TemplateContent {

  implicit object SingleJourneyTemplateContent extends TemplateContent[DummyControllerClass, SingleJourney] {
    val key: String = "single-journey"
  }

  implicit object BulkJourneyTemplateContent extends TemplateContent[DummyControllerClass, BulkJourney] {
    val key: String = "bulk-journey"
  }
}
