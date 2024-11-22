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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_details_are_correct
import play.api.data.Form
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm

trait CheckBankDetailsMixin extends JourneyBaseController {

  val postAction: Call
  def continueRoute(journey: Journey): Call
  val changeBankAccountDetailsRoute: Call
  val enterBankAccountDetailsRoute: Call
  val checkBankDetailsAreCorrectPage: check_bank_details_are_correct
  def isCMA(journey: Journey): Boolean = false

  def modifyJourney(journey: Journey, bankAccountDetails: BankAccountDetails): Either[String, Journey]
  def modifyJourneyRemoveBankDetails(journey: Journey): Journey

  final val bankDetailsAreYouSureForm: Form[YesNo] =
    YesOrNoQuestionForm("bank-details")

  final val showWarning: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      journey.computeBankAccountDetails
        .map { bankAccountDetails: BankAccountDetails =>
          modifyJourney(journey, bankAccountDetails)
            .fold(
              _ => (journey, Redirect(continueRoute(journey))),
              journeyWithBankDetails =>
                (
                  journeyWithBankDetails,
                  Ok(
                    checkBankDetailsAreCorrectPage(
                      bankDetailsAreYouSureForm,
                      bankAccountDetails.masked,
                      isCMA(journey),
                      postAction,
                      changeBankAccountDetailsRoute
                    )
                  )
                )
            )
        }
        .getOrElse {
          (
            journey,
            Redirect(
              if (journey.needsBanksAccountDetailsSubmission)
                enterBankAccountDetailsRoute
              else
                continueRoute(journey)
            )
          )
        }
        .asFuture
    }

  final val submitWarning: Action[AnyContent] =
    simpleActionReadWriteJourney { implicit request => journey =>
      bankDetailsAreYouSureForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              journey.answers.bankAccountDetails
                .map { bankAccountDetails =>
                  BadRequest(
                    checkBankDetailsAreCorrectPage(
                      formWithErrors,
                      bankAccountDetails.masked,
                      isCMA(journey),
                      postAction,
                      changeBankAccountDetailsRoute
                    )
                  )
                }
                .getOrElse(InternalServerError)
            ),
          answer =>
            (
              journey,
              Redirect(answer match {
                case YesNo.Yes => continueRoute(journey)
                case YesNo.No  => changeBankAccountDetailsRoute
              })
            )
        )
    }
}
