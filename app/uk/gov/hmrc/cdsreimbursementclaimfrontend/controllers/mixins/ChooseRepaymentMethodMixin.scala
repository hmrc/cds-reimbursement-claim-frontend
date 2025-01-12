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

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_repayment_method

import scala.concurrent.Future

trait ChooseRepaymentMethodMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with journeys.JourneyBase with journeys.SingleVariantProperties

  def postAction: Call
  def enterBankDetailsRoute: Call
  def chooseRepaymentMethodPage: choose_repayment_method

  def modifyJourney(journey: Journey, method: ReimbursementMethod): Either[String, Journey]
  def resetReimbursementMethod(journey: Journey): Journey

  val form: Form[ReimbursementMethod] =
    Forms.reimbursementMethodForm("reimbursement-method")

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => implicit journey =>
      (
        if journey.isAllSelectedDutiesAreCMAEligible then {
          Ok(
            chooseRepaymentMethodPage(
              form.withDefault(journey.answers.reimbursementMethod),
              postAction
            )
          )
        } else Redirect(enterBankDetailsRoute)
      ).asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney(
      implicit request =>
        journey =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  journey,
                  BadRequest(
                    chooseRepaymentMethodPage(
                      formWithErrors,
                      postAction
                    )
                  )
                ).asFuture,
              method =>
                modifyJourney(journey, method) match {
                  case Right(modifiedJourney) =>
                    (
                      modifiedJourney,
                      Redirect(enterBankDetailsRoute)
                    ).asFuture

                  case Left("submitReimbursementMethod.notCMAEligible") =>
                    (
                      journey,
                      Redirect(enterBankDetailsRoute)
                    ).asFuture

                  case Left(error) =>
                    Future.failed(new Exception(error))
                }
            ),
      fastForwardToCYAEnabled = false
    )

  final val reset: Action[AnyContent] =
    actionReadWriteJourney { _ => journey =>
      val updatedJourney =
        if !journey.isAllSelectedDutiesAreCMAEligible then resetReimbursementMethod(journey)
        else journey
      (updatedJourney, Redirect(checkYourAnswers)).asFuture
    }
}
