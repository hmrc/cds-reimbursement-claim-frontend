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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckBankDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkBankAccountDetailsPage: pages.check_bank_account_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  def show(): Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      val bankAccountTypeRoute = routes.ChooseBankAccountTypeController.show()
      // when the journey has come from the CYA page from the choose-repayment-type page and the user has previously select CMA
      // the journey is not complete because the bank details and bank account type have not been chosen, this happens on submitting the page
      //val continueActionCall   = if (journey.hasCompleteAnswers) checkYourAnswers else routes.ChooseFileTypeController.show()

      val continueActionCall = routes.ChooseFileTypeController.show()

      journey.getBankAccountDetails
        .map { bankAccountDetails: BankAccountDetails =>
          Future.successful(
            Ok(
              checkBankAccountDetailsPage(
                bankAccountDetails,
                continueActionCall,
                bankAccountTypeRoute
              )
            )
          )
        }
        .getOrElse {
          (Redirect(bankAccountTypeRoute)).asFuture
        }
    }
}
