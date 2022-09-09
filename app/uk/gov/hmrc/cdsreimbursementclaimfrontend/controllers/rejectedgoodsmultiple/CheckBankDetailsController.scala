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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{common => pages}

import scala.concurrent.ExecutionContext

@Singleton
class CheckBankDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkBankAccountDetailsPage: pages.check_bank_account_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  def show(): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      val bankAccountTypeRoute = routes.ChooseBankAccountTypeController.show()
      val continueRoute        =
        if (journey.userHasSeenCYAPage) checkYourAnswers
        else routes.ChooseFileTypeController.show()

      journey.computeBankAccountDetails
        .map { bankAccountDetails: BankAccountDetails =>
          journey
            .submitBankAccountDetails(bankAccountDetails)
            .fold(
              _ => (journey, Redirect(continueRoute)),
              journeyWithBankDetails =>
                (
                  journeyWithBankDetails,
                  Ok(
                    checkBankAccountDetailsPage(
                      bankAccountDetails.masked,
                      continueRoute,
                      bankAccountTypeRoute
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
                bankAccountTypeRoute
              else
                continueRoute
            )
          )
        }
        .asFuture
    }
}
