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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import java.util.UUID
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

trait WorkInProgressMixin[Journey] {
  self: JourneyBaseController[Journey] =>

  val show: Action[AnyContent] =
    simpleActionReadJourney { journey =>
      Ok(s"Work in progress ...\n\nJourney state:\n\n${prettyPrint(journey)}")
    }

  def show(a: Any): Action[AnyContent]         = show
  def show(a: Any, b: Any): Action[AnyContent] = show

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      (
        journey,
        Ok(
          s"Submitted form data:\n\n${request.asInstanceOf[Request[AnyContent]].body.asFormUrlEncoded.map(_.mkString("\n")).getOrElse("<empty>")}\n\nJourney state:\n\n${prettyPrint(journey)}"
        )
      ).asFuture
    }

  def submit(a: Any): Action[AnyContent]         = submit
  def submit(a: Any, b: Any): Action[AnyContent] = submit

  val redirectToALF: Action[AnyContent]                                   = show
  def retrieveAddressFromALF(id: Option[UUID] = None): Action[AnyContent] = show
  val reset: Action[AnyContent]                                           = show
  val summary: Action[AnyContent]                                         = show
  val showConfirmation: Action[AnyContent]                                = show

}
