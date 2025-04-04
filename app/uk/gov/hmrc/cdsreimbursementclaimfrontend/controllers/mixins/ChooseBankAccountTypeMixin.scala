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
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.bankAccountTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_bank_account_type

@Singleton
trait ChooseBankAccountTypeMixin extends JourneyBaseController {

  val postAction: Call
  val enterBankAccountDetailsRoute: Call
  val chooseBankAccountTypePage: choose_bank_account_type
  def isCMA(journey: Journey): Boolean = false

  def modifyJourney(journey: Journey, bankAccountType: BankAccountType): Either[String, Journey]

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      chooseBankAccountTypePage(
        bankAccountTypeForm.withDefault(journey.answers.bankAccountType),
        isCMA(journey),
        postAction
      )
    ).asFuture
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      journey =>
        bankAccountTypeForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(chooseBankAccountTypePage(formWithErrors, isCMA(journey), postAction))
              ).asFuture,
            bankAccountType =>
              modifyJourney(journey, bankAccountType)
                .fold(
                  e => {
                    logger.warn(e)
                    (journey, Redirect(baseRoutes.IneligibleController.ineligible))
                  },
                  updatedJourney => (updatedJourney, Redirect(enterBankAccountDetailsRoute))
                )
                .asFuture
          ),
    fastForwardToCYAEnabled = false
  )
}
