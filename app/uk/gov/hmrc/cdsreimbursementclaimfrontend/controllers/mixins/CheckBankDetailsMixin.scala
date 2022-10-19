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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_account_details

trait CheckBankDetailsMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with JourneyBase with CommonJourneyProperties

  def continueRoute(journey: Journey): Call
  val chooseBankAccountTypeRoute: Call
  val changeBankAccountDetailsRoute: Call
  val checkBankAccountDetailsPage: check_bank_account_details

  def modifyJourney(journey: Journey, bankAccountDetails: BankAccountDetails): Either[String, Journey]

  final val show: Action[AnyContent] =
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
                    checkBankAccountDetailsPage(
                      bankAccountDetails.masked,
                      continueRoute(journey),
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
                chooseBankAccountTypeRoute
              else
                continueRoute(journey)
            )
          )
        }
        .asFuture
    }
}
