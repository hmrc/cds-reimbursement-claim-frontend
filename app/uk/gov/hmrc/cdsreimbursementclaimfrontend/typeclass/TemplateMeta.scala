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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{DummyControllerClass, EnterClaimController, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType.{Bulk, Schedule, Single}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

trait TemplateMeta[C <: FrontendController] {
  def getKey(claimType: ClaimType): String
  def submitUrlFor: Call = routes.DummyControllerClass.testSubmit()
}

object TemplateMeta {

  implicit object DummyControllerClassTemplateMeta extends TemplateMeta[DummyControllerClass] {
    def getKey(claimType: ClaimType): String = claimType match {
      case Bulk     => "bulk-journey"
      case Schedule => "schedule-journey"
      case Single   => "single-journey"
    }
  }

  implicit object EnterClaimControllerClassTemplateMeta extends TemplateMeta[EnterClaimController] {
    def getKey(claimType: ClaimType): String = claimType match {
      case Bulk     => "bulk-journey"
      case Schedule => "schedule-journey"
      case Single   => "single-journey"
    }
  }

  object syntax {

    implicit class ClaimTypeTemplateMetaOps(val claimType: ClaimType) extends AnyVal {
      def showPage[T <: FrontendController](
        f: (String, Call) => Result
      )(implicit templateMeta: TemplateMeta[T]): Result = {
        val key = templateMeta.getKey(claimType)
        val url = templateMeta.submitUrlFor
        f(key, url)
      }
    }
  }
}
