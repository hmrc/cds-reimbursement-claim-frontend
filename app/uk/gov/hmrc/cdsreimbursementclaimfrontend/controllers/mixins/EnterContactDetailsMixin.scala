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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_or_change_contact_details

import scala.concurrent.Future

trait EnterContactDetailsMixin extends JourneyBaseController {

  val postAction: Call

  val continueRoute: Call

  val enterOrChangeContactDetailsPage: enter_or_change_contact_details

  def modifyJourney(journey: Journey, contactDetails: Option[MrnContactDetails]): Journey

  final def show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      Future.successful(
        Ok(
          enterOrChangeContactDetailsPage(
            Forms.mrnContactDetailsForm.withDefault(journey.answers.contactDetails),
            postAction
          )
        )
      )
    }

  final def submit: Action[AnyContent] =
    actionReadWriteJourney(implicit request =>
      journey =>
        Forms.mrnContactDetailsForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Future.successful(
                (
                  journey,
                  BadRequest(
                    enterOrChangeContactDetailsPage(
                      formWithErrors,
                      postAction
                    )
                  )
                )
              ),
            contactDetails => {
              val previousDetails =
                journey.answers.contactDetails
              val updatedJourney  = modifyJourney(
                journey,
                Some(
                  contactDetails
                    .computeChanges(previousDetails)
                )
              )
              Future.successful((updatedJourney, Redirect(continueRoute)))
            }
          )
    )
}
