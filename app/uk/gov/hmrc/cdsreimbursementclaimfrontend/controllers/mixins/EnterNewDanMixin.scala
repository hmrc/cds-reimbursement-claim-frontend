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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.newDanForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_new_dan

import scala.concurrent.Future

trait EnterNewDanMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with journeys.JourneyBase with journeys.OverpaymentsJourneyProperties

  val newDanPage: enter_new_dan
  val postAction: Call
  val continueAction: Call
  val formKey: String = "enter-new-dan"

  def modifyJourney(journey: Journey, dan: Dan): Journey

  def getNewDanAnswer(journey: Journey): Option[Dan] =
    journey.answers.newDan

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        newDanPage(
          newDanForm(formKey).withDefault(getNewDanAnswer(journey)),
          postAction
        )
      )
    }
  }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      newDanForm(formKey)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  newDanPage(
                    formWithErrors,
                    postAction
                  )
                )
              )
            ),
          dan =>
            Future.successful(
              (
                modifyJourney(journey, dan),
                Redirect(continueAction)
              )
            )
        )
    }
}
