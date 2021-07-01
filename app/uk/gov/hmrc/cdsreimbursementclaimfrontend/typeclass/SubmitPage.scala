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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{DummyControllerClass, routes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.ClaimType
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.ClaimType.{Single, Bulk}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.annotation.implicitNotFound

trait SubmitPage[F <: FrontendController, J <: ClaimType] {
  val nextUrl: String
}

object SubmitPage {

  implicit object SingleJourneySubmitPage extends SubmitPage[DummyControllerClass, ClaimType.Single.type] {
    val nextUrl: String = routes.NextPageController.nextSinglePage().url
  }

  implicit object BulkJourneySubmitPage extends SubmitPage[DummyControllerClass, ClaimType.Bulk.type] {
    val nextUrl: String = routes.NextPageController.nextBulkPage().url
  }

  object syntax {

    implicit class SubmitPageJourneyOps(val claimType: ClaimType) extends AnyVal {

      def getNextUrl[F <: FrontendController]: String = {

        @implicitNotFound("No submit page implicit found")
        def getFor[C <: FrontendController, J <: ClaimType](j: J)(implicit page: SubmitPage[C, J]): String = {
          println(j) // TODO: unused
          page.nextUrl
        }

        claimType match {
          case singleJourney: ClaimType.Single.type => getFor(singleJourney)
          case bulkJourney: ClaimType.Bulk.type     => getFor(bulkJourney)
        }
      }
    }
  }
}
