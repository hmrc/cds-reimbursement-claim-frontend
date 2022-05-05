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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.bankAccountTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}
import scala.concurrent.ExecutionContext

@Singleton
class ChooseBankAccountTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseBankAccountType: pages.choose_bank_account_type_page
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController
    with Logging {

  private val postAction: Call = routes.ChooseBankAccountTypeController.submit()

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      chooseBankAccountType(
        bankAccountTypeForm.withDefault(journey.answers.bankAccountType),
        postAction
      )
    ).asFuture
  }

  def submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      bankAccountTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(chooseBankAccountType(formWithErrors, postAction))
            ).asFuture,
          bankAccountType =>
            journey
              .submitBankAccountType(bankAccountType)
              .fold(
                error => {
                  logger.warn(error)
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                updatedJourney => (updatedJourney, Redirect(routes.EnterBankAccountDetailsController.show()))
              )
              .asFuture
        )
    },
    fastForwardToCYAEnabled = false
  )
}
