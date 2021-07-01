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

import play.api.mvc.{Call, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.Journey.{BulkJourney, SingleJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{DummyControllerClass, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.Journey
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.annotation.implicitNotFound

trait TemplateContent[C <: FrontendController, T <: Journey] {
  val key: String
  def submitUrlFor: Call = routes.DummyControllerClass.testSubmit()
}

object TemplateContent {

  implicit object SingleJourneyTemplateContent extends TemplateContent[DummyControllerClass, SingleJourney] {
    val key: String = "single-journey"
  }

  implicit object BulkJourneyTemplateContent extends TemplateContent[DummyControllerClass, BulkJourney] {
    val key: String = "bulk-journey"
  }

  object syntax {

    implicit class JourneyTemplateContentOps(val journey: Journey) extends AnyVal {

      def showPage[T <: FrontendController](f: (String, Call) => Result): Result = {

        @implicitNotFound("No template content implicit found")
        def bind[C <: FrontendController, J <: Journey](j: J)(implicit template: TemplateContent[C, J]) = {
          println(j) // TODO: this is unused
          f(template.key, template.submitUrlFor)
        }

        journey match {
          case singleJourney: SingleJourney => bind(singleJourney)
          case bulkJourney: BulkJourney     => bind(bulkJourney)
        }
      }
    }
  }
}
