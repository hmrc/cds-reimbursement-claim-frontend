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

import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_new_eori_number

import scala.concurrent.Future

trait EnterNewEoriNumberMixin extends JourneyBaseController {

  type Journey <: journeys.Journey & journeys.JourneyBase & journeys.OverpaymentsJourneyProperties

  val eoriDetailsConnector: EoriDetailsConnector
  val newEoriPage: enter_new_eori_number
  val postAction: Call
  val continueAction: Call
  val formKey: String = "enter-new-eori-number"

  def modifyJourney(journey: Journey, eori: Eori): Journey

  def getNewEoriAnswer(journey: Journey): Option[Eori] =
    journey.answers.newEori

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        newEoriPage(
          eoriNumberForm(formKey).withDefault(getNewEoriAnswer(journey)),
          postAction
        )
      )
    }
  }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      eoriNumberForm(formKey)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  newEoriPage(
                    formWithErrors,
                    postAction
                  )
                )
              )
            ),
          eori =>
            eoriDetailsConnector.getEoriDetails(eori).flatMap {
              case Some(_) =>
                Future.successful((modifyJourney(journey, eori), Redirect(continueAction)))
              case None    =>
                Future.successful(
                  (
                    journey,
                    BadRequest(
                      newEoriPage(
                        eoriNumberForm(formKey)
                          .fill(eori)
                          .withError(FormError("enter-new-eori-number", "doesNotExist")),
                        postAction
                      )
                    )
                  )
                )
            }
        )
    }
}
