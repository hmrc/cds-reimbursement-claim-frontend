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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.northernIrelandForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.claim_northern_ireland

import scala.concurrent.Future

trait OverpaymentsNorthernIrelandMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with journeys.JourneyBase with journeys.OverpaymentsJourneyProperties

  def modifyJourney(journey: Journey, whetherNorthernIreland: Boolean): Journey

  val northernIrelandAnswerPage: claim_northern_ireland
  val postAction: Call
  val continueRoute: Call

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      val form: Form[YesNo] =
        northernIrelandForm.withDefault(journey.answers.whetherNorthernIreland.map(YesNo.of))
      Ok(northernIrelandAnswerPage(form, None, postAction)).asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      northernIrelandForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  northernIrelandAnswerPage(formWithErrors, None, postAction)
                )
              )
            ),
          whetherNorthernIreland =>
            Future.successful(
              (
                modifyJourney(journey, whetherNorthernIreland.asBoolean),
                Redirect(continueRoute)
              )
            )
        )
    }
}
