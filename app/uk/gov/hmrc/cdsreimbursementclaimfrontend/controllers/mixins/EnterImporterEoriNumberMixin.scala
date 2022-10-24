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
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_importer_eori_number

import scala.concurrent.Future

trait EnterImporterEoriNumberMixin extends JourneyBaseController {

  val postAction: Call
  val continueAction: Call
  val whenEoriInputNotRequiredAction: Call
  val enterImporterEoriNumber: enter_importer_eori_number

  def modifyJourney(journey: Journey, eori: Eori): Either[String, Journey]

  val eoriNumberFormKey: String = "enter-importer-eori-number"

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      if (!journey.needsDeclarantAndConsigneeEoriSubmission)
        Redirect(whenEoriInputNotRequiredAction)
      else
        Ok(
          enterImporterEoriNumber(
            eoriNumberForm(eoriNumberFormKey).withDefault(journey.answers.consigneeEoriNumber),
            postAction
          )
        )
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    if (!journey.needsDeclarantAndConsigneeEoriSubmission)
      (journey, Redirect(whenEoriInputNotRequiredAction)).asFuture
    else {
      eoriNumberForm(eoriNumberFormKey)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  enterImporterEoriNumber(
                    formWithErrors.fill(Eori("")),
                    postAction
                  )
                )
              )
            ),
          eori =>
            Future.successful(
              modifyJourney(journey, eori)
                .fold(
                  errors => {
                    logger.error(s"Unable to record $eori - $errors")
                    (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                  },
                  updatedJourney =>
                    (
                      updatedJourney,
                      Redirect(continueAction)
                    )
                )
            )
        )
    }
  }
}
