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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.syntax.all._
import com.google.inject.Inject
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.bankAccountTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{common => pages}

import scala.concurrent.ExecutionContext

class ChooseBankAccountTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseBankAccountTypePage: pages.choose_bank_account_type_page
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController
    with Logging {

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      chooseBankAccountTypePage(
        bankAccountTypeForm.withDefault(journey.answers.bankAccountType),
        routes.ChooseBankAccountTypeController.submit()
      )
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      bankAccountTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(chooseBankAccountTypePage(formWithErrors, routes.ChooseBankAccountTypeController.submit()))
            ).asFuture,
          bankAccountType =>
            journey
              .submitBankAccountType(bankAccountType)
              .map((_, Redirect(routes.EnterBankAccountDetailsController.show())))
              .valueOr { error =>
                logger warn error
                (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
              }
              .asFuture
        )
    },
    fastForwardToCYAEnabled = false
  )
}
