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
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.bankAccountTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{common => pages}

@Singleton
trait ChooseBankAccountTypeMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with JourneyBase with CommonJourneyProperties

  val postAction: Call
  val enterBankAccountDetailsRoute: Call
  val chooseBankAccountTypePage: pages.choose_bank_account_type_page

  def modifyJourney(journey: Journey, bankAccountType: BankAccountType): Either[String, Journey]

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      chooseBankAccountTypePage(
        bankAccountTypeForm.withDefault(journey.answers.bankAccountType),
        postAction
      )
    ).asFuture
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      bankAccountTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(chooseBankAccountTypePage(formWithErrors, postAction))
            ).asFuture,
          bankAccountType =>
            modifyJourney(journey, bankAccountType)
              .fold(
                e => {
                  logger.warn(e)
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                updatedJourney => (updatedJourney, Redirect(enterBankAccountDetailsRoute))
              )
              .asFuture
        )
    },
    fastForwardToCYAEnabled = false
  )
}
